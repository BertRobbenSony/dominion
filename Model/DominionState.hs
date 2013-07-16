{-# LANGUAGE DeriveGeneric,TypeSynonymInstances,FlexibleInstances #-}
module Model.DominionState (
  Player,
  playerName,

  PlayerState (..),
  updatePlayerHand,
  updatePlayerDraw,
  updatePlayerDiscard,

  Board,

  GameState,
  board,
  updateBoard,
  actions,
  updateActions,
  money,
  updateMoney,
  buy,
  updateBuy,
  players,
  playerState,
  gotoNextPlayer,
  updatePlayerState,
  table, 
  updateTable
) where

import Prelude
import Data.Text (Text, unpack)
import GHC.Generics

--
-- Player
--
data Player = Player { playerName :: Text } deriving (Generic)

instance Eq Player where
  p1 == p2 = (playerName p1) == (playerName p2)

instance Show Player where
  show p = show $ playerName p

--
-- Board
-- 
type Board c = [(c, Int)]

----------------------------
  
data PlayerState c = PlayerState { 
  playerHand :: [c], 
  playerDraw :: [c], 
  playerDiscard :: [c] } deriving (Generic)

data GameState c = GameState { 
  actions :: Int, 
  money :: Int, 
  buy :: Int, 
  table :: [c],
  board :: Board c, 
  playerStates :: [(Player,PlayerState c)] } deriving (Generic)

updateTable :: ([c] -> [c]) -> GameState c -> GameState c
updateTable f gs = GameState (actions gs) (money gs) (buy gs) (f (table gs)) (board gs) (playerStates gs)

updateBoard :: (Board c -> Board c) -> GameState c -> GameState c
updateBoard f gs = GameState (actions gs) (money gs) (buy gs) (table gs) (f (board gs)) (playerStates gs)

updatePlayerState ::  Player -> (PlayerState c -> PlayerState c) -> GameState c -> GameState c
updatePlayerState p f gs = GameState (actions gs) (money gs) (buy gs) (table gs) (board gs) (updateList p f (playerStates gs))

initialState :: [(Text, [c])] -> Board c -> GameState c
initialState ps b = GameState 0 0 0 [] b (map toPlayerStates ps)
  where toPlayerStates (n,cards) = (Player n, PlayerState [] [] cards) 

updateActions :: (Int -> Int) -> GameState c -> GameState c
updateActions f gs = GameState (f (actions gs)) (money gs) (buy gs) (table gs) (board gs) (playerStates gs)

updateMoney :: (Int -> Int) -> GameState c -> GameState c
updateMoney f gs = GameState (actions gs) (f (money gs)) (buy gs) (table gs) (board gs) (playerStates gs)

updateBuy :: (Int -> Int) -> GameState c -> GameState c
updateBuy f gs = GameState (actions gs) (money gs) (f (buy gs)) (table gs) (board gs) (playerStates gs)

players :: GameState c -> [Player]
players gs = map fst (playerStates gs)

playerState :: Player -> GameState c -> PlayerState c
playerState p gs = getFromList p (playerStates gs)

gotoNextPlayer :: GameState c -> GameState c
gotoNextPlayer gs = GameState 1 0 1 [] (board gs) (shift $ playerStates gs)
  where shift (a:as) = as ++ [a]
  
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

---
--- helper functions
---

getFromList :: (Eq a) => a -> [(a,b)] -> b
getFromList a ((aa,bb):abs) = if a == aa then bb else getFromList a abs

updateList :: (Eq a) => a -> (b -> b) -> [(a,b)] -> [(a,b)]
updateList a f abs = map (\(aa,b) -> (aa, if a == aa then f b else b)) abs
