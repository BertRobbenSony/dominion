{-# LANGUAGE DeriveGeneric,TypeSynonymInstances,FlexibleInstances #-}
module Model.GameState (
) where

import Prelude
import Data.Text (Text, unpack)
import GHC.Generics
import Data.Aeson as Aeson
import Control.Monad
import Model.Game

data UserInput = UserInput String

-- choose :: [Card] -> Either String (GamePlay [Card])
-- decide :: Bool -> Either String (GamePlay [Card])

---------------------------------
newtype GamePlay a b = GamePlay { playGame :: GameState a -> (b,GameState a) }

data GameState a = GameState a (Maybe (String -> GameState a -> GameState a))

instance Monad (GamePlay a) where
  return a = GamePlay (\gs -> (a, gs))
  g >>= f  = GamePlay (\gs -> let (a,gs') = playGame g gs
                          in playGame (f a) gs')

instance Functor (GamePlay a) where
  fmap f gp = gp >>= (\a -> return $ f a)

updateState :: (a -> a) -> GameState a -> GameState a
updateState f (GameState a Nothing) = GameState (f a) Nothing
updateState f (GameState a (Just g)) = GameState a (Just g')
  where g' ui gs = updateState f (g ui gs)

updateStateWithUI:: (String -> GameState a -> GameState a) -> GameState a -> GameState a
updateStateWithUI f (GameState a Nothing) = GameState a (Just f)
updateStateWithUI f (GameState a (Just g)) = GameState a (Just g')
  where g' ui gs = updateStateWithUI f (g ui gs)

continue :: String -> GameState a -> GameState a
continue s (GameState a (Just f)) = f s (GameState a Nothing)
 