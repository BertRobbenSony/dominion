{-# LANGUAGE DeriveGeneric,TypeSynonymInstances,FlexibleInstances #-}
module Model.GamePlay (
  GamePlay,
  choose,
  decide,
  endGame,
  getState,
  updateState,
    
  Game,
  play,
  targetPlayer,
  playerMessage,
  isWaitingForPlayerDecision,
  playerDecision,
  playerChoiceOptions,
  playerChoice,
  scores,
  
  upTo
  
) where

import Prelude
import Data.Text (Text, unpack)
import GHC.Generics
import Control.Monad
import Model.Player

---
--- GamePlay
---

newtype GamePlay s c a = 
  GamePlay { playGame :: s -> (Either a (UserInteraction s c a), s) }

data UserInteraction s c a =  
  Choice String Player [c] ([c] -> Maybe String) ([c] -> GamePlay s c a) | 
  Decision String Player (Bool -> GamePlay s c a) |
  GameOver [(Player, Int)]
  
instance Monad (GamePlay s c) where
  return a = GamePlay (\s -> (Left a,s))
  f >>= g = GamePlay (\s -> case playGame f s of
                    (Left a, s') -> playGame (g a) s'
                    (Right u, s') -> (Right $ chain u g, s')) 

chain :: UserInteraction s c a -> (a -> GamePlay s c b) -> UserInteraction s c b
chain (Choice msg p cards v f) g = Choice msg p cards v f'
  where f' cs = (f cs) >>= g
chain (Decision msg p f) g = Decision msg p f'
  where f' cs = (f cs) >>= g
chain (GameOver s) g = GameOver s
  
choose :: String -> Player -> [c] -> ([c] -> Maybe String) -> GamePlay s c [c]
choose _ _ [] _ = return []
choose msg p cards v = GamePlay (\s -> (Right $ Choice msg p cards v f, s))
  where f cs = return cs

decide :: String -> Player -> GamePlay s c Bool
decide msg p = GamePlay (\s -> (Right $ Decision msg p f, s))
  where f b = return b

instance Functor (GamePlay s c) where
  fmap = liftM

endGame :: [(Player, Int)] -> GamePlay s c ()
endGame scores = GamePlay (\s -> (Right $ GameOver scores, s))

---
--- Game a
---
newtype Game s c = Game (UserInteraction s c (), s)

instance (Show s, Show c) => Show (Game s c) where
  show g = show (targetPlayer g) ++ ": " ++ playerMessage g ++
              ".\n" ++ show (gameState g)
           where gameState (Game (_, gs)) = gs 

play :: s -> GamePlay s c () -> Game s c
play gs gp = case playGame gp gs of
  (Right ui, gs') -> Game (ui, gs')

targetPlayer :: Game s c -> Player
targetPlayer (Game (Choice _ p _ _ _, _)) = p
targetPlayer (Game (Decision _ p _, _)) = p

playerMessage :: (Show c) => Game s c -> String
playerMessage (Game (Choice msg _ cards _ _, _)) = msg ++ ". Select a subset from " ++ show cards
playerMessage (Game (Decision msg _ _, _)) = msg

isWaitingForPlayerDecision :: Game s c -> Bool
isWaitingForPlayerDecision (Game (Decision _ _ _, _)) = True
isWaitingForPlayerDecision _ = True

playerDecision :: Bool -> Game s c -> Either String (Game s c)
playerDecision d (Game (Decision _ _ f, gs)) = Right $ play gs (f d)
playerDecision _ _ = Left "You're not in a state to decide"

playerChoiceOptions :: Game s c -> Maybe [c]
playerChoiceOptions (Game (Choice _ _ options _ _, _)) = Just options
playerChoiceOptions _ = Nothing

playerChoice :: (Eq c) => [c] -> Game s c -> Either String (Game s c)
playerChoice choice (Game (Choice _ _ cards constraint f, gs)) =
  if (choice `isSubset` cards) 
    then maybe (Right $ play gs (f choice)) Left (constraint choice)
    else Left "Please select a subset of the given options"
  where [] `isSubset` _ = True
        (a:as) `isSubset` bs = maybe False (\bb -> isSubset as bb) (safeWithout a bs)
        safeWithout a [] = Nothing
        safeWithout a (b:bs) = if a == b then Just bs else (safeWithout a bs) >>= (\bb -> Just (b:bb))   
playerChoice _ _ = Left "You're not in a state to choose."

scores :: Game s c -> Maybe [(Player, Int)]
scores (Game (GameOver s, _)) = Just s
scores _ = Nothing

---
--- GamePlay a
---

getState :: GamePlay s c s
getState = GamePlay (\s -> (Left s, s))

updateState :: (s -> s) -> GamePlay s c ()
updateState f = GamePlay (\s -> (Left (), f s))

upTo :: Int -> [a] -> Maybe String
upTo n cs = if length cs <= n then Nothing else Just $ "Choose up to " ++ (show n) ++ " cards please."
