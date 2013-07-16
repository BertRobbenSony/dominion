{-# LANGUAGE DeriveGeneric,TypeSynonymInstances,FlexibleInstances #-}
module Model.Game (

  Board,
  
  Player,
  playerName,
  
  GameState,  
  
  board,
  players,
  table,
  hand,
  drawSize,
  
  takeCardFromHand,
  putCardInHand,
  discardCard,
  discardCardFromHand,
  drawCard,
  drawCards,
  drawCardAndPutInHand,
  takeCardFromBoard,
  takeCardFromTable,
  boardCardValue,
  trashCard,
  putCardOnTopOfDeck,
  putCardOnTable
  
) where

import Prelude
import Data.Text (Text, unpack)
import GHC.Generics
import Data.Aeson as Aeson
import Control.Monad

type Board c = [(c, Int)]

--
-- Player
--
data Player = Player { playerName :: Text } deriving (Generic)

instance Eq Player where
  p1 == p2 = (playerName p1) == (playerName p2)

instance Show Player where
  show p = show $ playerName p
----------------------------
  
data PlayerState c = PlayerState { 
  playerHand :: [c], 
  playerDraw :: [c], 
  playerDiscard :: [c] } deriving (Generic)

------------------------------

type PlayerStates c = [(Player,PlayerState c)]  

data GameState c = GameState { 
  table :: [c],
  board :: Board c, 
  playerStates :: PlayerStates c} deriving (Generic)

updateTable :: ([c] -> [c]) -> GameState c -> GameState c
updateTable f gs = GameState (f (table gs)) (board gs) (playerStates gs)

updateBoard :: (Board c -> Board c) -> GameState c -> GameState c
updateBoard f gs = GameState (table gs) (f (board gs)) (playerStates gs)

updatePlayer ::  Player -> (PlayerState c -> PlayerState c) -> GameState c -> GameState c
updatePlayer p f gs = GameState (table gs) (board gs) (updateList p f (playerStates gs))

-----

players :: GameState c -> [Player]
players gs = map fst (playerStates gs)

hand :: Player -> GameState c -> [c]
hand p gs = playerHand $ getFromList p (playerStates gs)

trashCard :: c -> GameState c -> GameState c
trashCard _ gs = gs

drawSize :: Player -> GameState c -> Int
drawSize p gs = length (playerDraw $ getFromList p (playerStates gs))

takeCardFromHand :: (Eq c) => c -> Player -> GameState c -> GameState c
takeCardFromHand c p = updatePlayer p $ updatePlayerHand (without c)

putCardInHand :: c -> Player -> GameState c -> GameState c
putCardInHand c p = updatePlayer p $ updatePlayerHand (c:)

putCardOnTable :: c -> GameState c -> GameState c
putCardOnTable c = updateTable (c:)

discardCard :: c -> Player -> GameState c -> GameState c
discardCard c p = updatePlayer p $ updatePlayerDiscard (c:)

discardCardFromHand :: (Eq c) => c -> Player -> GameState c -> GameState c
discardCardFromHand c p = (takeCardFromHand c p) . (discardCard c p)

drawCard :: Player -> GameState c -> (Maybe c, GameState c)
drawCard p gs = let 
   ps = getFromList p (playerStates gs)
   (mc,ps') = playerDrawCard ps
   in (mc, updatePlayer p (\_ -> ps') gs)   

drawCards :: Player -> Int -> GameState c -> ([c], GameState c)
drawCards p n gs = let
   ps = getFromList p (playerStates gs)
   (cs,ps') = playerDrawCards n ps
   in (cs, updatePlayer p (\_ -> ps') gs)   

drawCardAndPutInHand :: Player -> GameState c -> GameState c
drawCardAndPutInHand p gs = case drawCard p gs of
  (Nothing, gs') -> gs'
  (Just c, gs') -> putCardInHand c p gs'
                          
takeCardFromBoard :: (Eq c) => c -> GameState c -> GameState c
takeCardFromBoard c = updateBoard (updateList c (\n->n-1)) 

takeCardFromTable :: (Eq c) => c -> GameState c -> GameState c
takeCardFromTable c = updateTable (without c)

putCardOnTopOfDeck :: c -> Player -> GameState c -> GameState c
putCardOnTopOfDeck c p = updatePlayer p (updatePlayerDraw (c:)) 

takeFromBoard :: (Eq c) => c -> Board c -> Board c
takeFromBoard c1 bo@((c2,n):b) = if c1 == c2 
  then if n == 0 then bo else (c2,n-1):b
  else (c2,n) : (takeFromBoard c1 b)

boardCardValue :: (Eq c) => c -> GameState c -> Maybe Int
boardCardValue c gs = lookup c (board gs)

---
--- Player state changes
---

updatePlayerHand :: ([c] -> [c]) -> PlayerState c -> PlayerState c
updatePlayerHand f ps = PlayerState { 
  playerHand = f (playerHand ps), 
  playerDraw = playerDraw ps, 
  playerDiscard = playerDiscard ps }

updatePlayerDraw :: ([c] -> [c]) -> PlayerState c -> PlayerState c
updatePlayerDraw f ps = PlayerState { 
  playerHand = playerHand ps, 
  playerDraw = f (playerDraw ps), 
  playerDiscard = playerDiscard ps }

updatePlayerDiscard :: ([c] -> [c]) -> PlayerState c -> PlayerState c
updatePlayerDiscard f ps = PlayerState { 
  playerHand = playerHand ps, 
  playerDraw = playerDraw ps, 
  playerDiscard = f (playerDiscard ps) }

-- todo add shuffle  
playerDrawCard :: PlayerState c -> (Maybe c, PlayerState c) 
playerDrawCard ps = case (playerDraw ps) of
  c:cs -> (Just c, updatePlayerDraw (\_ -> cs) ps)
  [] -> case playerDiscard ps of
      [] -> (Nothing, ps)
      c:cs -> (Just c, PlayerState { playerHand = playerHand ps, playerDraw = cs, playerDiscard = [] })

playerDrawCards :: Int -> PlayerState c -> ([c], PlayerState c) 
playerDrawCards 0 ps = ([], ps) 
playerDrawCards n ps = case (playerDraw ps) of
  c:cs -> let (cards, ps') = playerDrawCards (n-1) (updatePlayerDraw (\_ -> cs) ps) in (c:cards, ps')
  [] -> case playerDiscard ps of
      [] -> ([], ps)
      cs -> playerDrawCards n (PlayerState { playerHand = playerHand ps, playerDraw = cs, playerDiscard = [] })

playerDrawAndAddToHand :: PlayerState c -> PlayerState c
playerDrawAndAddToHand ps = case playerDrawCard ps of
  (Nothing, ps') -> ps'
  (Just c, ps') -> updatePlayerHand (c:) ps'
  
takeFirst :: (Eq a) => a -> [a] -> [a]
takeFirst c1 (c2:cs) = if c1 == c2 then cs else c1 : takeFirst c1 cs
 


without :: (Eq a) => a -> [a] -> [a]
without c1 (c2:cs) = if c1 == c2 then cs else c2:(without c1 cs)
                        
getFromList :: (Eq a) => a -> [(a,b)] -> b
getFromList a ((aa,bb):abs) = if a == aa then bb else getFromList a abs

updateList :: (Eq a) => a -> (b -> b) -> [(a,b)] -> [(a,b)]
updateList a f abs = map (\(aa,b) -> (aa, if a == aa then f b else b)) abs