{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
module Playground where

import Prelude

class Combinator a where
  (<.>) :: a -> a -> a

newtype IntToString = IntToString (Int -> String)

type ItoS = Int -> String
  
instance Combinator IntToString where
  (IntToString f) <.> (IntToString g) = IntToString $ \n -> (f n) ++ "," ++ (g n) 
  
instance Combinator ItoS where
  f <.> g = \n -> (f n) ++ "," ++ (g n) 

instance (Combinator a) => Combinator (b -> a) where
  f <.> g = \n -> (f n) <.> (g n) 

instance Combinator String where
  s1 <.> s2 = s1 ++ s2
