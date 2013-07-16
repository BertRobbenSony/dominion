module Model.Playing

where

import Model.Dominion
import Model.DominionState
import Model.Card

playTurn :: GamePlay ()
playTurn = do
  playActions
  buyCards
  cleanUp

gotoNextPlayer :: GamePlay ()
gotoNextPlayer = do
  ps <- players

  