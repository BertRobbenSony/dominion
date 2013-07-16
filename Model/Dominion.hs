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
  hand,
  currentBoard,
  increaseActions,
  increaseMoney,
  increaseBuys,
  chooseCards,
  decide,
  drawSize,
  drawCard,
  drawCardAndPutInHand,
  putCardOnTopOfDeck,
  putCardOnTable,
  trashCard,
  discardCard,
  takeCardFromBoard,
  takeCardFromHand,
  takeCardFromTable,
  discardCardFromHand,
  drawCards,
  attack,
    
  Game,
  play,
  targetPlayer,
  playerMessage,
  playerDecision,
  playerChoice,
  
  upTo
  
) where

import Prelude
import Data.Text (Text, unpack)
import GHC.Generics
import Control.Monad
import Model.DominionState

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

---
--- GamePlay
---

newtype GamePlay a = 
  GamePlay { playGame :: GameState Card -> (Either a (UserInteraction a), GameState Card) }

data UserInteraction a = 
  ChooseCards String Player [Card] ([Card] -> Maybe String) ([Card] -> GamePlay a) | 
  Decide String Player (Bool -> GamePlay a)
  
instance Monad GamePlay where
  return a = GamePlay (\s -> (Left a,s))
  f >>= g = GamePlay (\s -> case playGame f s of
                    (Left a, s') -> playGame (g a) s'
                    (Right u, s') -> (Right $ chain u g, s')) 

chain :: UserInteraction a -> (a -> GamePlay b) -> UserInteraction b
chain (ChooseCards msg p cards v f) g = ChooseCards msg p cards v f'
  where f' cs = (f cs) >>= g
chain (Decide msg p f) g = Decide msg p f'
  where f' cs = (f cs) >>= g
  
chooseCards :: String -> Player -> [Card] -> ([Card] -> Maybe String) -> GamePlay [Card]
chooseCards msg p cards v = GamePlay (\s -> (Right $ ChooseCards msg p cards v f, s))
  where f cs = return cs

decide :: String -> Player -> GamePlay Bool
decide msg p = GamePlay (\s -> (Right $ Decide msg p f, s))
  where f b = return b
 
instance Functor GamePlay where
  fmap f gp = gp >>= (\a -> return $ f a)

---
--- Game a
---
newtype Game a = Game (UserInteraction a, GameState Card)

play :: GameState Card -> GamePlay a -> Game a
play gs gp = case playGame gp gs of
  (Right ui, gs') -> Game (ui, gs')

targetPlayer :: Game a -> Player
targetPlayer (Game (ChooseCards _ p _ _ _, _)) = p
targetPlayer (Game (Decide _ p _, _)) = p

playerMessage :: Game a -> String
playerMessage (Game (ChooseCards msg _ cards _ _, _)) = msg ++ ". Select cards from " ++ show cards
playerMessage (Game (Decide msg _p _, _)) = msg

playerDecision :: Bool -> Game a -> Either String (Game a)
playerDecision = undefined

playerChoice :: [Card] -> Game a -> Either String (Game a)
playerChoice choice (Game (ChooseCards _ _ cards constraint f, gs)) =
  if (choice `isSubset` cards) then
    maybe (Right $ play gs (f choice)) Left (constraint choice)
  else Left "Please select a subset of the given cards" 
playerChoice _ _ = Left "You're not in a state to choose cards."

---
--- GamePlay a
---

simpleGetter :: (GameState Card -> a) -> GamePlay a
simpleGetter f = GamePlay (\s -> (Left (f s), s))

currentBoard :: GamePlay (Board Card)
currentBoard = simpleGetter board
  
currentActions :: GamePlay Int
currentActions = simpleGetter actions

currentMoney :: GamePlay Int
currentMoney = simpleGetter money

currentBuys :: GamePlay Int
currentBuys = simpleGetter buy

updateState :: (GameState Card -> GameState Card) -> GamePlay ()
updateState f = GamePlay (\s -> (Left (), f s))

increaseActions :: Int -> GamePlay ()
increaseActions n = updateState $ updateActions (+ n)

increaseMoney :: Int -> GamePlay ()
increaseMoney n = updateState $ updateMoney (+ n)

increaseBuys :: Int -> GamePlay ()
increaseBuys n = updateState $ updateBuy (+ n)

hand :: Player -> GamePlay [Card]
hand p = simpleGetter (\s -> playerHand $ playerState p s)

trashCard :: Card -> GamePlay ()
trashCard _ = return ()

drawSize :: Player -> GamePlay Int
drawSize p = simpleGetter (\gs -> length (playerHand $ playerState p gs))

takeCardFromHand :: Card -> Player -> GamePlay ()
takeCardFromHand c p = updateState $ updatePlayerState p $ updatePlayerHand (without c)

putCardInHand :: Card -> Player -> GamePlay ()
putCardInHand c p = updateState $ updatePlayerState p $ updatePlayerHand (c:)

putCardOnTable :: Card -> GamePlay ()
putCardOnTable c = updateState $ updateTable (c:)

discardCard :: Card -> Player -> GamePlay ()
discardCard c p = updateState $ updatePlayerState p $ updatePlayerDiscard (c:)

discardCardFromHand :: Card -> Player -> GamePlay ()
discardCardFromHand c p = (takeCardFromHand c p) >> (discardCard c p)

drawCard :: Player -> GamePlay (Maybe Card)
drawCard p = do
  ps <- simpleGetter $ playerState p  
  let (mc, ps') = playerDrawCard ps
  updateState $ updatePlayerState p (\_ -> ps')
  return mc

-- todo add shuffle  
playerDrawCard :: PlayerState c -> (Maybe c, PlayerState c) 
playerDrawCard ps = case (playerDraw ps) of
  c:cs -> (Just c, updatePlayerDraw (\_ -> cs) ps)
  [] -> case playerDiscard ps of
      [] -> (Nothing, ps)
      c:cs -> (Just c, PlayerState { playerHand = playerHand ps, playerDraw = cs, playerDiscard = [] })

drawCards :: Player -> Int -> GamePlay [Card]
drawCards p n = do
   ps <- simpleGetter $ playerState p
   let (cs,ps') = playerDrawCards n ps
   updateState $ updatePlayerState p (\_ -> ps')
   return cs

drawCardAndPutInHand :: Player -> GamePlay ()
drawCardAndPutInHand p = do
  mc <- drawCard p
  maybe (return ()) (\c -> putCardInHand c p) mc
                          
takeCardFromBoard :: Card -> GamePlay ()
takeCardFromBoard c = updateState $ updateBoard (updateList c (\n->n-1)) 

takeCardFromTable :: Card -> GamePlay()
takeCardFromTable c = updateState $ updateTable (without c)

putCardOnTopOfDeck :: Card -> Player -> GamePlay ()
putCardOnTopOfDeck c p = updateState $ updatePlayerState p (updatePlayerDraw (c:)) 

takeFromBoard :: (Eq c) => c -> Board c -> Board c
takeFromBoard c1 bo@((c2,n):b) = if c1 == c2 
  then if n == 0 then bo else (c2,n-1):b
  else (c2,n) : (takeFromBoard c1 b)

---
--- Player state changes
---

playerDrawCards :: Int -> PlayerState c -> ([c], PlayerState c) 
playerDrawCards 0 ps = ([], ps) 
playerDrawCards n ps = case (playerDraw ps) of
  c:cs -> let (cards, ps') = playerDrawCards (n-1) (updatePlayerDraw (\_ -> cs) ps) in (c:cards, ps')
  [] -> case playerDiscard ps of
      [] -> ([], ps)
      cs -> playerDrawCards n (PlayerState { playerHand = playerHand ps, playerDraw = cs, playerDiscard = [] })

playerDrawAndAddToHand :: PlayerState c -> PlayerState c
playerDrawAndAddToHand ps = case playerDrawCard ps of
  (Nothing, ps') -> ps'
  (Just c, ps') -> updatePlayerHand (c:) ps'
  
attack :: (Player -> GamePlay a) -> GamePlay [a]
attack f = do
  ps <- simpleGetter $ players
  forM ps f
         
---
--- Game progress
---

playTurn :: GamePlay ()
playTurn = do
  playActions
  buyCards
  cleanUp

playActions :: GamePlay ()
playActions = do
  p <- fmap head (simpleGetter players)
  h <- hand p
  cards <- chooseCards "Choose action card to play" p h (upTo 1)
  unless (null cards) $ do
    increaseActions (-1)
    takeCardFromHand (head cards) p
    putCardOnTable (head cards)
    cardGamePlay (head cards) p
    actions <- currentActions
    when (actions > 0) playActions

buyCards :: GamePlay ()
buyCards = undefined

cleanUp :: GamePlay ()
cleanUp = do
  p <- fmap head (simpleGetter players)  
  t <- simpleGetter table
  updateState $ updateTable (\_ -> [])
  h <- hand p
  updateState $ updatePlayerState p $ updatePlayerHand (\_ -> [])
  updateState $ updatePlayerState p $ updatePlayerDiscard (\d -> h ++ t ++ d)
  forM [1..5] (\_ -> drawCardAndPutInHand p)
  return ()

---
--- helper functions
---

takeFirst :: (Eq a) => a -> [a] -> [a]
takeFirst c1 (c2:cs) = if c1 == c2 then cs else c1 : takeFirst c1 cs
 
without :: (Eq a) => a -> [a] -> [a]
without c1 (c2:cs) = if c1 == c2 then cs else c2:(without c1 cs)
                        
safeWithout :: (Eq a) => a -> [a] -> Maybe [a]
safeWithout a [] = Nothing
safeWithout a (b:bs) = if a == b then Just bs else (safeWithout a bs) >>= (\bb -> Just (b:bb))
 
getFromList :: (Eq a) => a -> [(a,b)] -> b
getFromList a ((aa,bb):abs) = if a == aa then bb else getFromList a abs

updateList :: (Eq a) => a -> (b -> b) -> [(a,b)] -> [(a,b)]
updateList a f abs = map (\(aa,b) -> (aa, if a == aa then f b else b)) abs

isSubset :: (Eq a) => [a] -> [a] -> Bool
[] `isSubset` _ = True
(a:as) `isSubset` bs = maybe False (\bb -> isSubset as bb) (safeWithout a bs)

upTo :: Int -> [a] -> Maybe String
upTo n cs = if length cs <= n then Nothing else Just $ "Choose up to " ++ (show n) ++ " cards please."
