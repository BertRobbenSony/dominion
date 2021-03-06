{-# LANGUAGE DeriveGeneric,TypeSynonymInstances,FlexibleInstances #-}
module Model.Player (

  Player(..)
    
) where

import Prelude
import GHC.Generics
import Data.Text (Text, unpack)

data Player = Player { playerName :: Text, pid :: Int } deriving (Generic)

instance Show Player where
  show p = unpack $ playerName p

instance Eq Player where
  p1 == p2 = (pid p1) == (pid p2)

  
