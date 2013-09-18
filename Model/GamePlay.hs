{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
module Model.GamePlay (
  GamePlay,
  getState,
  updateState,
  userInteraction,
    
  Game,
  play,
  gameState,
  userInput
  
) where

import Prelude
import Control.Monad

---
--- GamePlay
---

newtype GamePlay s u a = 
  GamePlay { playGame :: s -> (Either a (u (GamePlay s u a)), s) }

instance Functor u => Monad (GamePlay s u) where
  return a = GamePlay (\s -> (Left a,s))
  f >>= g = GamePlay (\s -> case playGame f s of
                    (Left a, s') -> playGame (g a) s'
                    (Right u, s') -> (Right $ fmap (\gpa -> gpa >>= g) u, s')) 


getState :: GamePlay s u s
getState = GamePlay (\s -> (Left s, s))

updateState :: (s -> s) -> GamePlay s u ()
updateState f = GamePlay (\s -> (Left (), f s))

userInput :: u (GamePlay s u a) -> GamePlay s u a
userInput u = GamePlay (\s -> (Right $ u, s))

instance Functor u => Functor (GamePlay s u) where
  fmap = liftM

---
--- Game a
---

newtype Game s u = Game (u (GamePlay s u ()), s)

--instance (Show s, Show (u a)) => Show (Game s u) where
--  show g = show (userInteraction g) ++ ".\n" ++ show (gameState g)

play :: s -> GamePlay s u () -> Game s u
play gs gp = case playGame gp gs of
  (Right ui, gs') -> Game (ui, gs')

userInteraction :: Game s u -> u (GamePlay s u ())
userInteraction (Game (u, _)) = u

gameState :: Game s u -> s
gameState (Game (_, s)) = s

