{-# LANGUAGE DeriveGeneric,TypeSynonymInstances,FlexibleInstances,OverloadedStrings #-}
module Model.Game (

  Game,
  join,
  newGame,
  dominionGame,
  liftDominionGame
    
) where

import Prelude
import Model.Player
import Model.DominionGamePlay
import Model.Card
import Model.GameState
import Data.Text (Text)

newtype Game = Game (Either (Int, Int, [Player]) DominionGame)

instance Show Game where
  show (Game g) = either (\(n,_,ps) -> "Waiting for " ++ (show n) ++ " players to join."
                                        ++ "\n" 
                                        ++ "Current players: " ++ (show ps))
                         (\_ -> "Game in progress.")
                         g 
  
newGame :: Int -> Int -> Game
newGame n seed = Game $ Left (n, seed, [])

join :: Player -> Game -> Either Text Game
join p (Game (Left startUp)) = Right $ Game $ addPlayer p startUp 
join _ _ = Left "Game is in progress. It's no longer possible to join."

addPlayer :: Player -> (Int, Int, [Player]) -> Either (Int, Int, [Player]) DominionGame 
addPlayer p (n,s,ps) = if n == 1 then Right (simpleGame s (p:ps)) else Left (n-1, s, p:ps)
                                          
simpleGame :: Int -> [Player] -> DominionGame
simpleGame seed ps = newDominionGame simpleState [province] where
  startDeck = [estate, estate, estate, copper, copper, copper, copper, copper, copper, copper]
  simpleBoard = [(copper, 50), (silver, 30), (gold, 20), 
                 (village, 10), (chapel, 10), 
                 (estate, 8), (duchy, 8), (province, 8)]
  simpleState = initialState seed (map (\p -> (p, startDeck)) ps) simpleBoard

dominionGame :: Game -> Maybe DominionGame
dominionGame (Game (Right dg)) = Just dg
dominionGame _ = Nothing

liftDominionGame :: (DominionGame -> Either Text DominionGame) -> Game -> Either Text Game
liftDominionGame f (Game (Right dg)) = fmap withDominionGame (f dg) where
    withDominionGame d = Game (Right d)
liftDominionGame _ _ = Left "Illegal state -- game is not started."