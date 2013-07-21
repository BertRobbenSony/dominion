{-# LANGUAGE DeriveGeneric,TypeSynonymInstances,FlexibleInstances #-}
module Model.DominionGamePlay (
  Card,
  card,
  cardValue,
  cardGamePlay,
  isTreasure,
  isAction,
  isVictory,
  CardType (..),
  DominionGamePlay,
  currentActions,
  currentMoney,
  currentBuys,
  hand,
  currentBoard,
  increaseActions,
  increaseMoney,
  increaseBuys,
  drawSize,
  drawCard,
  drawCardAndPutInHand,
  putCardInHand,
  putCardOnTopOfDeck,
  putCardOnTable,
  trashCard,
  discardCard,
  takeCardFromBoard,
  takeCardFromHand,
  takeCardFromTable,
  discardCardFromHand,
  drawCards,
  allPlayers,
  
  newGame
    
) where

import Prelude
import Data.Text (Text, unpack)
import GHC.Generics
import Control.Monad
import Model.Player
import Model.GameState
import Model.GamePlay

--
-- Card
--
data Card = Card { 
  cardName :: String, 
  cardValue :: Int, 
  cardTypes :: [CardType] }

card :: String -> Int -> [CardType] -> Card
card = Card

instance Show Card where                                                                                       
    show c = cardName c                                                                            

instance Eq Card where
  c1 == c2 = (cardName c1) == (cardName c2)
      

data CardType = Victory ([Card] -> Int) | 
  Treasure (Player -> DominionGamePlay()) | 
  Action (Player -> DominionGamePlay()) |
  Reaction (DominionGamePlay () -> Player -> DominionGamePlay ()) | 
  Attack (Player -> Player -> DominionGamePlay ())

type DominionGamePlay = GamePlay (GameState Card) Card

victoryPoints :: Card -> [Card] -> Int 
victoryPoints = undefined

cardGamePlay :: Card -> Player -> DominionGamePlay ()
cardGamePlay c p = playAction c p >> playAttack c p

playAction :: Card -> Player -> DominionGamePlay ()
playAction c p = forM (cardTypes c) cardTypeGamePlay >> return()
  where cardTypeGamePlay (Action a) = a p
        cardTypeGamePlay _ = return ()

playReaction :: Card -> DominionGamePlay () -> Player -> DominionGamePlay ()
playReaction c p a = forM (cardTypes c) cardTypeGamePlay >> return()
  where cardTypeGamePlay (Reaction f) = f p a
        cardTypeGamePlay _ = return ()

playTreasure :: Card -> Player -> DominionGamePlay ()
playTreasure c p = forM (cardTypes c) cardTypeGamePlay >> return()
  where cardTypeGamePlay (Treasure a) = a p
        cardTypeGamePlay _ = return ()

playAttack :: Card -> Player -> DominionGamePlay ()
playAttack c p = forM (cardTypes c) cardTypeGamePlay >> return()
  where cardTypeGamePlay (Attack a) = attack a p
        cardTypeGamePlay _ = return ()

isTreasure :: Card -> Bool
isTreasure c = any isTreasureType (cardTypes c)
  where isTreasureType (Treasure _) = True
        isTreasureType _ = False

isAction :: Card -> Bool
isAction c = any isActionType (cardTypes c)
  where isActionType (Action _) = True
        isActionType _ = False

isReaction :: Card -> Bool
isReaction c = any isReactionType (cardTypes c)
  where isReactionType (Reaction _) = True
        isReactionType _ = False

isVictory :: Card -> Bool
isVictory c = any isVictoryType (cardTypes c)
  where isVictoryType (Victory _) = True
        isVictoryType _ = False

---
--- Game a
---

newGame :: GameState Card -> [Card] -> Game (GameState Card) Card
newGame gs endCards = play gs (startGame endCards)

instance Show (GameState Card) where
  show gs = "Hand: " ++ show (playerHand $ playerState (head (players gs)) gs) ++ 
            "\nActions: " ++ show (actions gs) ++ ", money: " ++ show (money gs) ++ ", buys: " ++ show (buy $ gs) ++ 
            "\nBoard: " ++ show (board gs) ++ 
            "\nTable: " ++ show (table gs)
  
---
--- DominionGamePlay a
---

simpleGetter :: (GameState Card -> a) -> DominionGamePlay a
simpleGetter f = fmap f getState

currentBoard :: DominionGamePlay (Board Card)
currentBoard = simpleGetter board
  
currentActions :: DominionGamePlay Int
currentActions = simpleGetter actions

currentMoney :: DominionGamePlay Int
currentMoney = simpleGetter money

currentBuys :: DominionGamePlay Int
currentBuys = simpleGetter buy

currentPlayer :: DominionGamePlay Player
currentPlayer = fmap head (simpleGetter players)

allPlayers :: DominionGamePlay [Player]
allPlayers = simpleGetter players

increaseActions :: Int -> DominionGamePlay ()
increaseActions n = updateState $ updateActions (+ n)

increaseMoney :: Int -> DominionGamePlay ()
increaseMoney n = updateState $ updateMoney (+ n)

increaseBuys :: Int -> DominionGamePlay ()
increaseBuys n = updateState $ updateBuy (+ n)

hand :: Player -> DominionGamePlay [Card]
hand p = simpleGetter (\s -> playerHand $ playerState p s)

trashCard :: Card -> DominionGamePlay ()
trashCard _ = return ()

drawSize :: Player -> DominionGamePlay Int
drawSize p = simpleGetter (\gs -> length (playerHand $ playerState p gs))

takeCardFromHand :: Card -> Player -> DominionGamePlay ()
takeCardFromHand c p = updateState $ updatePlayerState p $ updatePlayerHand (without c)

putCardInHand :: Card -> Player -> DominionGamePlay ()
putCardInHand c p = updateState $ updatePlayerState p $ updatePlayerHand (c:)

putCardOnTable :: Card -> DominionGamePlay ()
putCardOnTable c = updateState $ updateTable (c:)

discardCard :: Card -> Player -> DominionGamePlay ()
discardCard c p = updateState $ updatePlayerState p $ updatePlayerDiscard (c:)

discardCardFromHand :: Card -> Player -> DominionGamePlay ()
discardCardFromHand c p = (takeCardFromHand c p) >> (discardCard c p)

drawCard :: Player -> DominionGamePlay (Maybe Card)
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

drawCards :: Player -> Int -> DominionGamePlay [Card]
drawCards p n = do
   ps <- simpleGetter $ playerState p
   let (cs,ps') = playerDrawCards n ps
   updateState $ updatePlayerState p (\_ -> ps')
   return cs

drawCardAndPutInHand :: Player -> DominionGamePlay ()
drawCardAndPutInHand p = do
  mc <- drawCard p
  maybe (return ()) (\c -> putCardInHand c p) mc
                          
takeCardFromBoard :: Card -> DominionGamePlay ()
takeCardFromBoard c = updateState $ updateBoard (updateList c (\n->n-1)) 

takeCardFromTable :: Card -> DominionGamePlay()
takeCardFromTable c = updateState $ updateTable (without c)

putCardOnTopOfDeck :: Card -> Player -> DominionGamePlay ()
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
  
attack :: (Player -> Player -> DominionGamePlay ()) -> Player -> DominionGamePlay ()
attack f attacker = do
  ps <- simpleGetter players
  forM (tail ps) $ \victim -> do
    h <- hand victim
    reaction <- choose "Choose reaction card to play" victim (filter isReaction h) (upTo 1)
    if null reaction 
      then f attacker victim
      else playReaction (head reaction) (f attacker victim) victim      
  return ()
         
---
--- Game progress
---

startGame :: [Card] -> DominionGamePlay ()
startGame endCards = do
  resetCounters
  ps <- simpleGetter players
  forM ps (\p -> forM [1..5] (\_ -> drawCardAndPutInHand p))
  dominionGame endCards

dominionGame :: [Card] -> DominionGamePlay ()
dominionGame endCards = do
  playTurn
  gameOver <- hasEnded endCards
  unless gameOver $ do
    updateState gotoNextPlayer
    dominionGame endCards

hasEnded :: [Card] -> DominionGamePlay Bool
hasEnded endCards = fmap ended currentBoard
  where ended board = threeOrMoreEmptySpaces board || endCardsEmpty board
        threeOrMoreEmptySpaces board = length (filter (\(_,n) -> n == 0) board) >= 3
        endCardsEmpty board = 0 `elem` map (\c -> getFromList c board) endCards

playTurn :: DominionGamePlay ()
playTurn = do
  playActions
  playMoney
  buyCards
  cleanUp

playActions :: DominionGamePlay ()
playActions = do
  p <- currentPlayer
  h <- hand p
  cards <- choose "Choose action card to play" p (filter isAction h) (upTo 1)
  unless (null cards) $ do
    increaseActions (-1)
    takeCardFromHand (head cards) p
    putCardOnTable (head cards)
    cardGamePlay (head cards) p
    actions <- currentActions
    when (actions > 0) playActions

playMoney :: DominionGamePlay ()
playMoney = do
  p <- currentPlayer
  h <- hand p
  cards <- choose "Choose treasure card(s) to play" p (filter isTreasure h) (\_ -> Nothing)
  forM cards (\c -> do
    takeCardFromHand c p
    putCardOnTable c
    playTreasure c p)
  return ()

buyCards :: DominionGamePlay ()
buyCards = do
  p <- currentPlayer  
  b <- currentBuys
  m <- currentMoney
  board <- currentBoard
  cards <- choose ("Choose at most " ++ show b ++ " card(s) to buy costing at most " ++ show m) p (cardsInBudget board m) (canBuy b m board)
  forM cards (\c -> takeCardFromBoard c >> discardCard c p)
  return ()

cardsInBudget :: Board Card -> Int -> [Card]
cardsInBudget b budget = filter (\c -> cardValue c <= budget) $ map fst b

canBuy :: Int -> Int -> Board Card -> [Card] -> Maybe String
canBuy _ _ _ [] = Nothing
canBuy maxCount budget b (c:cs) = if maxCount < 1 
  then Just $ "You can only buy " ++ (show maxCount) ++ " cards"
  else if budget < cardValue c 
       then Just "You don't have enough money to buy all these cards"
       else if getFromList c b < 1 
            then Just "Not all cards are available in the board"
            else canBuy (maxCount - 1) (budget - cardValue c) (updateList c (\n->n-1) b) cs
  
cleanUp :: DominionGamePlay ()
cleanUp = do
  resetCounters
  p <- fmap head (simpleGetter players)  
  t <- simpleGetter table
  updateState $ updateTable (\_ -> [])
  h <- hand p
  updateState $ updatePlayerState p $ updatePlayerHand (\_ -> [])
  updateState $ updatePlayerState p $ updatePlayerDiscard (\d -> h ++ t ++ d)
  forM [1..5] (\_ -> drawCardAndPutInHand p)
  return ()

resetCounters :: DominionGamePlay ()
resetCounters = do
  updateState $ updateMoney (\_ -> 0)
  updateState $ updateBuy (\_ -> 1)
  updateState $ updateActions (\_ -> 1)
  
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
 
updateList :: (Eq a) => a -> (b -> b) -> [(a,b)] -> [(a,b)]
updateList a f abs = map (\(aa,b) -> (aa, if a == aa then f b else b)) abs
