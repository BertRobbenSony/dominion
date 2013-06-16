module Model.Playing

where

import Model.Game

data GameView = GameView

myHand :: GameView -> [Card]
myHand = undefined

table :: GameView -> [Card]
table = undefined

actions :: GameView -> Int
actions = undefined

myOpponents :: GameView -> [Opponent]
myOpponents = undefined

opponentHand :: Opponent -> Int
opponentHand = undefined

opponentName :: Opponent -> String
opponentName = undefined

buys :: GameView -> Int
buys = undefined

money :: GameView -> Int
money = undefined

chooseCards :: [Card] -> GameView -> Either String GameView
chooseCards = undefined

decide :: Bool -> GameView -> Either String GameView
decide = undefined

data GameState = CardInput String [Card] | DecisionInput String | Waiting Opponent | GameOver [(String, Int, Int)]

currentState :: GameView -> GameState
currentState = undefined
  