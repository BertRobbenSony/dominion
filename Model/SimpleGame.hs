module Model.SimpleGame

where

import Model.DominionGamePlay
import Model.GameState
import Model.GamePlay
import Model.Card
import Model.Player
import Data.Text (pack)

simpleGame :: Game (GameState Card) Card
simpleGame = newGame simpleState [province] where
  startDeck = [estate, estate, estate, copper, copper, copper, copper, copper, copper, copper]
  simpleBoard = [(copper, 50), (silver, 30), (gold, 20), 
                 (village, 10), (chapel, 10), 
                 (estate, 8), (duchy, 8), (province, 8)]
  simpleState = initialState [(pack "Alice", startDeck), (pack "Bob", startDeck)] simpleBoard

  