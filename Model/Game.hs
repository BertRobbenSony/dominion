{-# LANGUAGE DeriveGeneric,TypeSynonymInstances,FlexibleInstances #-}
module Model.Game (

  Board,
  
  Player,
  playerName,
  
  Game,  
  
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
  drawCardAndPutInHand,
  takeCardFromBoard,
  takeCardFromTable,
  boardCardValue,
  putCardOnTopOfDeck

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

mapTable :: ([c] -> [c]) -> GameState c -> GameState c
mapTable f gs = GameState (f (table gs)) (board gs) (playerStates gs)

mapBoard :: (Board c -> Board c) -> GameState c -> GameState c
mapBoard f gs = GameState (table gs) (f (board gs)) (playerStates gs)

mapPlayerState ::  Player -> (PlayerState c -> PlayerState c) -> GameState c -> GameState c
mapPlayerState p f gs = GameState (table gs) (board gs) (updateList p f (playerStates gs))

-----

newtype Game c a = Game { playGame :: GameState c -> (a, GameState c) }

instance Monad (Game c) where
  return a = Game (\gs -> (a, gs))
  g >>= f  = Game (\gs -> let (a,gs') = playGame g gs
                          in playGame (f a) gs')
  
players :: Game c [Player]
players = Game (\gs -> (map fst (playerStates gs), gs))

hand :: Player -> Game c [c]
hand p = Game (\gs -> (playerHand $ getFromList p (playerStates gs), gs))

drawSize :: Player -> Game c Int
drawSize p = Game (\gs -> (length (playerDraw $ getFromList p (playerStates gs)), gs))

takeCardFromHand :: (Eq c) => c -> Player -> Game c ()
takeCardFromHand c p = updatePlayer p $ updatePlayerHand (without c)

putCardInHand :: c -> Player -> Game c ()
putCardInHand c p = updatePlayer p $ updatePlayerHand (c:)

discardCard :: c -> Player -> Game c ()
discardCard c p = updatePlayer p $ updatePlayerDiscard (c:)

discardCardFromHand :: (Eq c) => c -> Player -> Game c ()
discardCardFromHand c p = (takeCardFromHand c p) >> (discardCard c p)

drawCard :: Player -> Game c (Maybe c)
drawCard p = Game (\gs -> let 
   ps = getFromList p (playerStates gs)
   (mc,ps') = playerDrawCard ps
   in (mc, mapPlayerState p (\_ -> ps') gs))   

drawCardAndPutInHand :: Player -> Game c ()
drawCardAndPutInHand p = do
                          c <- drawCard p
                          tryAndPutCardInHand c p
                          
tryAndPutCardInHand :: Maybe c -> Player -> Game c ()
tryAndPutCardInHand Nothing _ = return ()
tryAndPutCardInHand (Just c) p = putCardInHand c p

takeCardFromBoard :: (Eq c) => c -> Game c ()
takeCardFromBoard c = Game (\gs -> ((), mapBoard (updateList c (\n->n-1)) gs)) 

takeCardFromTable :: (Eq c) => c -> Game c ()
takeCardFromTable c = Game (\gs -> ((), mapTable (without c) gs))

putCardOnTopOfDeck :: c -> Player -> Game c ()
putCardOnTopOfDeck c p = updatePlayer p (updatePlayerDraw (c:)) 

updatePlayer :: Player -> (PlayerState c -> PlayerState c) -> Game c ()
updatePlayer p psf = Game (\gs -> ((), mapPlayerState p psf gs))

takeFromBoard :: (Eq c) => c -> Board c -> Board c
takeFromBoard c1 bo@((c2,n):b) = if c1 == c2 
  then if n == 0 then bo else (c2,n-1):b
  else (c2,n) : (takeFromBoard c1 b)

boardCardValue :: (Eq c) => c -> Game c (Maybe Int)
boardCardValue c = Game (\gs -> (lookup c (board gs), gs))

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