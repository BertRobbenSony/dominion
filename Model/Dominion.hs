{-# LANGUAGE DeriveGeneric,TypeSynonymInstances,FlexibleInstances #-}
module Model.Dominion (
  Card,
  card,
  cardValue,
  GamePlay,
  currentActions,
  currentMoney,
  currentBuys,
  updateActions,
  updateMoney,
  updateBuys,
  liftGame,
  cardsChoice,
  decision,
  cardsFromHand,
  maxCardsFromHand,
  attack
) where

import Prelude
import Data.Text (Text, unpack)
import GHC.Generics
import Data.Aeson as Aeson
import Control.Monad
import Model.Game

--
-- Card
--
data Card = Card { 
  cardName :: String, 
  cardValue :: Int, 
  victoryPoints :: [Card] -> Int, 
  cardActionPlay :: Player -> GamePlay () }

card :: String -> Int -> ([Card] -> Int) -> (Player -> GamePlay ()) -> Card
card = Card

instance Show Card where                                                                                       
    show c = cardName c                                                                            

instance Eq Card where
  c1 == c2 = (cardName c1) == (cardName c2)
      

data CardType = Victory | Treasure | Action | Reaction | Attack

------------------------------

newtype GamePlay a = GamePlay { playGame :: GameState -> (a,GameState) }

data GameState = GameState { actions :: Int, money :: Int, buy :: Int, game :: Game Card () }

instance Monad GamePlay where
  return a = GamePlay (\gs -> (a, gs))
  g >>= f  = GamePlay (\gs -> let (a,gs') = playGame g gs
                          in playGame (f a) gs')

instance Functor GamePlay where
  fmap f gp = gp >>= (\a -> return $ f a)
  
currentActions :: GamePlay Int
currentActions = undefined

currentMoney :: GamePlay Int
currentMoney = undefined

currentBuys :: GamePlay Int
currentBuys = undefined

updateActions :: Int -> GamePlay ()
updateActions = undefined

updateMoney :: Int -> GamePlay ()
updateMoney = undefined

updateBuys :: Int -> GamePlay Int
updateBuys = undefined

liftGame :: Game Card a -> GamePlay a
liftGame = undefined

cardsChoice :: String -> Player -> ([Card] -> GamePlay (Maybe String)) -> GamePlay [Card]
cardsChoice = undefined

decision :: String -> Player -> GamePlay Bool
decision = undefined

cardsFromHand :: Player -> [Card] -> GamePlay (Maybe String)
cardsFromHand p cards = do
    h <- liftGame $ hand p
    return $ if (cards `isSubset` h) then Nothing else Just "Please choose cards from hand"

maxCardsFromHand :: Player -> Int -> [Card] -> GamePlay (Maybe String)
maxCardsFromHand p n cards = if length cards > n 
    then return $ Just ("Please choose up to " ++ (show n) ++ " cards.")
    else cardsFromHand p cards

attack :: (Player -> GamePlay a) -> GamePlay [a]
attack = undefined
         
isSubset :: (Eq a) => [a] -> [a] -> Bool
[] `isSubset` _ = True
(a:as) `isSubset` bs = maybe False (\bb -> isSubset as bb) (safeWithout a bs)

safeWithout :: (Eq a) => a -> [a] -> Maybe [a]
safeWithout a [] = Nothing
safeWithout a (b:bs) = if a == b then Just bs else (safeWithout a bs) >>= (\bb -> Just (b:bb))
 