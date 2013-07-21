{-# LANGUAGE DeriveGeneric,TypeSynonymInstances,FlexibleInstances #-}
module Model.Player (

  Player(..)
    
) where

import Prelude
import GHC.Generics
import Data.Text (Text, unpack)

---
--- GamePlay
---

data Player = Player { playerName :: Text } deriving (Generic)

instance Show Player where
  show p = unpack $ playerName p
  
instance Eq Player where
  p1 == p2 = (playerName p1) == (playerName p2)

  
