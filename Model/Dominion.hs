{-# LANGUAGE DeriveGeneric,TypeSynonymInstances,FlexibleInstances #-}
module Model.Dominion (
  Card,
  card,
  cardValue,
  cardGamePlay,
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
  cardGamePlay :: Player -> GamePlay () }

card :: String -> Int -> ([Card] -> Int) -> (Player -> GamePlay ()) -> Card
card = Card

instance Show Card where                                                                                       
    show c = cardName c                                                                            

instance Eq Card where
  c1 == c2 = (cardName c1) == (cardName c2)
      

data CardType = Victory | Treasure | Action | Reaction | Attack

------------------------------
data DominionGame = DominionGame

data PlayState = CardChoice String Player [Card] ([Card] -> Either DominionGame String) |
                 Decision String Player (Bool -> Either DominionGame String) | 
                 GameOver [(Player, Int, Int)]

play :: DominionGame -> PlayState
play = undefined

players :: DominionGame -> [Player]
players = undefined

playerHand :: Player -> DominionGame -> [Card]
playerHand = undefined

---------------------------------
newtype GamePlay a = GamePlay { playGame :: GameState -> (a,GameState) }

data GameState = GameState { actions :: Int, money :: Int, buy :: Int, game :: Game Card () }

type NextState a = GameState -> (a, GameState)

data DominionPlay a = GamePlay (NextState a) |
                      Decision String Player (Bool -> (Either (NextState Bool) String)) |
                      CardChoice String [Card] Player ([Card] -> (Either (NextState [Card]) String))                 

cardsChoice :: String -> [Card] -> Player -> ([Card] -> Maybe String) -> GamePlay [Card]

instance Monad DominionPlay where
  return a = GamePlay (\gs -> (a, gs))
  (GamePlay gp) >>= f  = GamePlay (\gs -> let (a,gs') = gp gs
                          in playGame (f a) gs')
  (Decision msg p dpf) >>= f = Decision msg p (\b -> fmap (\dp -> dp >>= f) (dpf b))
  (CardChoice msg cards p cf) >>= f = CardChoice msg cards p (\cs -> fmap (\p -> p >>= f) (cf cs))

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

cardsChoice :: String -> [Card] -> Player -> ([Card] -> Maybe String) -> GamePlay [Card]
cardsChoice msg cards p validateCards = GamePlay (\gs -> CardChoice msg cards p

decision :: String -> Player -> GamePlay Bool
decision = undefined

attack :: (Player -> GamePlay a) -> GamePlay [a]
attack = undefined
         
isSubset :: (Eq a) => [a] -> [a] -> Bool
[] `isSubset` _ = True
(a:as) `isSubset` bs = maybe False (\bb -> isSubset as bb) (safeWithout a bs)

safeWithout :: (Eq a) => a -> [a] -> Maybe [a]
safeWithout a [] = Nothing
safeWithout a (b:bs) = if a == b then Just bs else (safeWithout a bs) >>= (\bb -> Just (b:bb))
 