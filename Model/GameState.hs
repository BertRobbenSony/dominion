{-# LANGUAGE DeriveGeneric,TypeSynonymInstances,FlexibleInstances #-}
module Model.GameState (

  PlayerState (..),
  updatePlayerHand,
  updatePlayerDraw,
  updatePlayerDiscard,

  Board,
  getFromList,

  GameState,
  initialState,
  board,
  updateBoard,
  actions,
  updateActions,
  money,
  updateMoney,
  buy,
  updateBuy,
  randomGen,
  updateRandomGen,
  players,
  playerState,
  gotoNextPlayer,
  updatePlayerState,
  table, 
  updateTable
) where

import Prelude
import GHC.Generics
import Model.Player
import System.Random

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
  playerStates :: [(Player,PlayerState c)],
  randomGen :: StdGen } deriving (Generic)

updateTable :: ([c] -> [c]) -> GameState c -> GameState c
updateTable f gs = gs { table = f (table gs) }

updateBoard :: (Board c -> Board c) -> GameState c -> GameState c
updateBoard f gs = gs { board = f (board gs) }

updatePlayerState ::  Player -> (PlayerState c -> PlayerState c) -> GameState c -> GameState c
updatePlayerState p f gs = gs { playerStates = updateList p f (playerStates gs) }

updateRandomGen :: StdGen -> GameState c -> GameState c
updateRandomGen r gs = gs { randomGen = r }

initialState :: Int -> [(Player, [c])] -> Board c -> GameState c
initialState seed ps b = GameState 0 0 0 [] b (map toPlayerStates ps) (mkStdGen seed)
  where toPlayerStates (p,cards) = (p, PlayerState [] [] cards) 

updateActions :: (Int -> Int) -> GameState c -> GameState c
updateActions f gs = gs { actions = f (actions gs) }

updateMoney :: (Int -> Int) -> GameState c -> GameState c
updateMoney f gs = gs { money = f (money gs) }

updateBuy :: (Int -> Int) -> GameState c -> GameState c
updateBuy f gs = gs { buy = f (buy gs) }

players :: GameState c -> [Player]
players gs = map fst (playerStates gs)

playerState :: Player -> GameState c -> PlayerState c
playerState p gs = getFromList p (playerStates gs)

gotoNextPlayer :: GameState c -> GameState c
gotoNextPlayer gs = GameState 1 0 1 [] (board gs) (shift $ playerStates gs) (randomGen gs)
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
getFromList a ((aa,bb):rest) = if a == aa then bb else getFromList a rest

updateList :: (Eq a) => a -> (b -> b) -> [(a,b)] -> [(a,b)]
updateList a f list = map (\(aa,b) -> (aa, if a == aa then f b else b)) list
