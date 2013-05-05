{-# LANGUAGE DeriveGeneric #-}
module Model.Game where

import Prelude
import Data.Text (Text)
import GHC.Generics
import Data.Aeson as Aeson
import Model.Card

data Game = Game { gameState :: GameState, board :: Board, players :: [Player] } deriving (Generic)
instance Aeson.ToJSON Game

data Player = Player { name :: Text, playerState :: PlayerState  } deriving (Generic)
instance Aeson.ToJSON Player

data GameState = WaitingForPlayers Int | 
	Finished | 
	Playing { player :: Int, playableActions :: [Action] } deriving (Generic)
instance Aeson.ToJSON GameState

newGame :: Int -> Game
newGame n = Game (WaitingForPlayers n) (initialBoard n) []

join :: Text -> Game -> Maybe Game
join p g = case gameState g of
	WaitingForPlayers 1 -> Just $ Game (Playing 0 []) (board g) ((Player p initialPlayerState):(players g))
	WaitingForPlayers n -> Just $ Game (WaitingForPlayers (n-1)) (board g) ((Player p initialPlayerState):(players g))
	_ -> Nothing

type Action = String

data PlayerState = PlayerState { hand :: [Card], 
	draw :: [Card], 
	discard :: [Card],
	table :: [Card],
	actions :: Int,
	money :: Int,
	buy :: Int } deriving (Generic)
instance Aeson.ToJSON PlayerState

instance Aeson.ToJSON Card where
  toJSON c = toJSON $ show c

initialPlayerState :: PlayerState
initialPlayerState = drawCard . drawCard . drawCard . drawCard . drawCard $ 
	PlayerState { hand = [], discard = initialDeck, draw = [],
		table = [], actions = 1, money = 0, buy = 1 }
		
-- todo add shuffle	
drawCard :: PlayerState -> PlayerState
drawCard ps = case (draw ps) of
  c:cs -> PlayerState { hand = c:(hand ps), draw = cs, discard = discard ps, table = table ps, 
                        actions = actions ps, money = money ps, buy = buy ps }
  [] -> case (discard ps) of
  		[] -> ps
  		c:cs -> PlayerState { hand = c:(hand ps), draw = cs, discard = [], table = table ps,
  		                      actions = actions ps, money = money ps, buy = buy ps }
   