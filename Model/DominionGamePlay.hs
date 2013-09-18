{-# LANGUAGE DeriveGeneric,TypeSynonymInstances,FlexibleInstances,OverloadedStrings #-}
module Model.DominionGamePlay (
  Card,
  card,
  cardName,
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
  choose,
  decide,
  upTo,
  
  newDominionGame,
  DominionGame,
  dominionGameState,
  targetPlayer,
  playerMessage,
  isWaitingForPlayerDecision,
  playerDecision,
  playerChoiceOptions,
  playerChoice,
  scores
      
) where

import Prelude
import Control.Monad
import Model.Player
import Model.GameState
import Model.GamePlay
import System.Random
import Data.Text (Text, unpack, pack, append)

-- Card
--
data Card = Card { 
  cardName :: Text, 
  cardValue :: Int, 
  cardTypes :: [CardType] }

card :: Text -> Int -> [CardType] -> Card
card = Card

instance Show Card where                                                                                       
    show c = unpack $ cardName c                                                                            

instance Eq Card where
  c1 == c2 = (cardName c1) == (cardName c2)
      

data CardType = Victory ([Card] -> Int) | 
  Treasure (Player -> DominionGamePlay()) | 
  Action (Player -> DominionGamePlay()) |
  Reaction (DominionGamePlay () -> Player -> DominionGamePlay ()) | 
  Attack (Player -> Player -> DominionGamePlay ())

type DominionGamePlay = GamePlay (GameState Card) UserInteraction

victoryPoints :: Card -> [Card] -> Int 
victoryPoints c cs = foldr points 0 (cardTypes c)
  where points (Victory f) _ = f cs
        points _ acc = acc

cardGamePlay :: Card -> Player -> DominionGamePlay ()
cardGamePlay c p = playAction c p >> playAttack c p

playAction :: Card -> Player -> DominionGamePlay ()
playAction c p = forM_ (cardTypes c) cardTypeGamePlay
  where cardTypeGamePlay (Action a) = a p
        cardTypeGamePlay _ = return ()

playReaction :: Card -> DominionGamePlay () -> Player -> DominionGamePlay ()
playReaction c p a = forM_ (cardTypes c) cardTypeGamePlay
  where cardTypeGamePlay (Reaction f) = f p a
        cardTypeGamePlay _ = return ()

playTreasure :: Card -> Player -> DominionGamePlay ()
playTreasure c p = forM_ (cardTypes c) cardTypeGamePlay
  where cardTypeGamePlay (Treasure a) = a p
        cardTypeGamePlay _ = return ()

playAttack :: Card -> Player -> DominionGamePlay ()
playAttack c p = forM_ (cardTypes c) cardTypeGamePlay
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
--- DominionGame
---

newtype DominionGame = DominionGame (Game (GameState Card) UserInteraction)

newDominionGame :: GameState Card -> [Card] -> DominionGame
newDominionGame gs endCards = DominionGame $ play gs (startGame endCards)

instance Show (GameState Card) where
  show gs = "Hand: " ++ show (playerHand $ playerState (head (players gs)) gs) ++ 
            "\nActions: " ++ show (actions gs) ++ ", money: " ++ show (money gs) ++ ", buys: " ++ show (buy $ gs) ++ 
            "\nBoard: " ++ show (board gs) ++ 
            "\nTable: " ++ show (table gs)

dominionGameState :: DominionGame -> GameState Card
dominionGameState (DominionGame g) = gameState g

targetPlayer :: DominionGame -> Player
targetPlayer (DominionGame g) = case userInteraction g of
  Choice _ p _ _ _ -> p
  Decision _ p _ -> p

playerMessage :: DominionGame -> Text
playerMessage (DominionGame g) = case userInteraction g of
  Choice msg _ cards _ _ -> msg `append` (pack (". Select a subset from " ++ (show cards)))
  Decision msg _ _ -> msg
  GameOver _ -> "This game has ended."

isWaitingForPlayerDecision :: DominionGame -> Bool
isWaitingForPlayerDecision (DominionGame g) = case userInteraction g of
  Decision _ _ _ -> True
  _              -> False

playerDecision :: Bool -> DominionGame -> Either Text DominionGame
playerDecision d (DominionGame g) = case userInteraction g of
  Decision _ _ f -> Right $ DominionGame $ play (gameState g) (f d)
  _              -> Left "You're not in a state to decide"

playerChoiceOptions :: DominionGame -> Maybe [Card]
playerChoiceOptions (DominionGame g) = case userInteraction g of
  Choice _ _ options _ _ -> Just options
  _                      -> Nothing

playerChoice :: [Card] -> DominionGame -> Either Text DominionGame
playerChoice choice (DominionGame g) = case userInteraction g of
  Choice _ _ cards constraint f ->
    if (choice `isSubset` cards) 
      then maybe (Right $ DominionGame $ play (gameState g) (f choice)) Left (constraint choice)
      else Left "Please select a subset of the given options"
    where [] `isSubset` _ = True
          (a:as) `isSubset` bs = maybe False (\bb -> isSubset as bb) (safeWithout a bs)
          safeWithout _ [] = Nothing
          safeWithout a (b:bs) = if a == b then Just bs else (safeWithout a bs) >>= (\bb -> Just (b:bb))   
  _ -> Left "You're not in a state to choose."

scores :: DominionGame -> Maybe [(Player, Int)]
scores (DominionGame g) = case userInteraction g of
  GameOver s -> Just s
  _ -> Nothing

  

------------------------------------

--- 
--- UserInteraction
---
data UserInteraction gp =  
  Choice Text Player [Card] ([Card] -> Maybe Text) ([Card] -> gp) | 
  Decision Text Player (Bool -> gp) |
  GameOver [(Player, Int)]

instance Functor UserInteraction where
  fmap f (Choice msg p cards v cont) = Choice msg p cards v (\cs -> f (cont cs))
  fmap f (Decision msg p cont) = Decision msg p (\cs -> f (cont cs))
  fmap _ (GameOver score) = GameOver score 

choose :: Text -> Player -> [Card] -> ([Card] -> Maybe Text) -> GamePlay s UserInteraction [Card]
choose _ _ [] _ = return []
choose msg p cards v = userInput $ Choice msg p cards v return

decide :: Text -> Player -> GamePlay s UserInteraction Bool
decide msg p = userInput $ Decision msg p return

endGame :: [(Player, Int)] -> GamePlay s UserInteraction ()
endGame finalScores = userInput $ GameOver finalScores


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
  rg <- simpleGetter $ randomGen
  let (rg1,rg2) = split rg  
  let (mc, ps') = playerDrawCard rg1 ps
  updateState $ updatePlayerState p (\_ -> ps')
  updateState $ updateRandomGen rg2
  return mc

playerDrawCard :: (RandomGen r) => r -> PlayerState c -> (Maybe c, PlayerState c) 
playerDrawCard r ps = case (playerDraw ps) of
  c:cs -> (Just c, updatePlayerDraw (\_ -> cs) ps)
  [] -> case playerDiscard ps of
      [] -> (Nothing, ps)
      cs -> let (c:rest) = shuffle r cs
            in (Just c, PlayerState { playerHand = playerHand ps, playerDraw = rest, playerDiscard = [] })

drawCards :: Player -> Int -> DominionGamePlay [Card]
drawCards _ 0 = return []
drawCards p n = do
  mc <- drawCard p
  case mc of
    Nothing -> return []
    Just c -> fmap (c:) (drawCards p (n-1))

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

---
--- Player state changes
---

attack :: (Player -> Player -> DominionGamePlay ()) -> Player -> DominionGamePlay ()
attack f attacker = do
  ps <- simpleGetter players
  forM_ (tail ps) $ \victim -> do
    h <- hand victim
    reaction <- choose "Choose reaction card to play" victim (filter isReaction h) (upTo 1)
    if null reaction 
      then f attacker victim
      else playReaction (head reaction) (f attacker victim) victim      
         
---
--- Game progress
---

startGame :: [Card] -> DominionGamePlay ()
startGame endCards = do
  resetCounters
  ps <- simpleGetter players
  forM_ ps (\p -> forM [1..5] (\_ -> drawCardAndPutInHand p))
  dominionGame endCards

dominionGame :: [Card] -> DominionGamePlay ()
dominionGame endCards = do
  playTurn
  gameOver <- hasEnded endCards
  if gameOver
    then countScores >>= endGame
    else do
      updateState gotoNextPlayer
      dominionGame endCards

countScores :: DominionGamePlay [(Player,Int)]
countScores = liftM countScoresFromState getState
  where countScoresFromState gs = map (playerScore gs) (players gs)
        playerScore gs p = let 
                  ps = playerState p gs
                  cs = playerHand ps ++ playerDraw ps ++ playerDiscard ps
                  count c acc = acc + victoryPoints c cs
                  in (p, foldr count 0 cs)

hasEnded :: [Card] -> DominionGamePlay Bool
hasEnded endCards = fmap ended currentBoard
  where ended brd = threeOrMoreEmptySpaces brd || endCardsEmpty brd
        threeOrMoreEmptySpaces brd = length (filter (\(_,n) -> n == 0) brd) >= 3
        endCardsEmpty brd = 0 `elem` map (\c -> getFromList c brd) endCards

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
    actionsLeft <- currentActions
    when (actionsLeft > 0) playActions

playMoney :: DominionGamePlay ()
playMoney = do
  p <- currentPlayer
  h <- hand p
  cards <- choose "Choose treasure card(s) to play" p (filter isTreasure h) (\_ -> Nothing)
  forM_ cards (\c -> do
    takeCardFromHand c p
    putCardOnTable c
    playTreasure c p)

buyCards :: DominionGamePlay ()
buyCards = do
  p <- currentPlayer  
  b <- currentBuys
  m <- currentMoney
  brd <- currentBoard
  cards <- choose (pack ("Choose at most " ++ show b ++ " card(s) to buy costing at most " ++ show m)) p (cardsInBudget brd m) (canBuy b m brd)
  forM_ cards (\c -> takeCardFromBoard c >> discardCard c p)

cardsInBudget :: Board Card -> Int -> [Card]
cardsInBudget b budget = filter (\c -> cardValue c <= budget) $ map fst b

canBuy :: Int -> Int -> Board Card -> [Card] -> Maybe Text
canBuy _ _ _ [] = Nothing
canBuy maxCount budget b (c:cs) = if maxCount < 1 
  then Just $ pack ("You can only buy " ++ show maxCount ++ " cards")
  else if budget < cardValue c 
       then Just "You don't have enough money to buy all these cards"
       else if getFromList c b < 1 
            then Just "Not all cards are available in the board"
            else canBuy (maxCount - 1) (budget - cardValue c) (updateList c (\n->n-1) b) cs
  
cleanUp :: DominionGamePlay ()
cleanUp = do
  resetCounters
  p <- liftM head (simpleGetter players)  
  t <- simpleGetter table
  updateState $ updateTable (\_ -> [])
  h <- hand p
  updateState $ updatePlayerState p $ updatePlayerHand (\_ -> [])
  updateState $ updatePlayerState p $ updatePlayerDiscard (\d -> h ++ t ++ d)
  forM_ [1..5] (\_ -> drawCardAndPutInHand p)

resetCounters :: DominionGamePlay ()
resetCounters = do
  updateState $ updateMoney (\_ -> 0)
  updateState $ updateBuy (\_ -> 1)
  updateState $ updateActions (\_ -> 1)
  
---
--- helper functions
---

without :: (Eq a) => a -> [a] -> [a]
without c1 (c2:cs) = if c1 == c2 then cs else c2:(without c1 cs)
                        
updateList :: (Eq a) => a -> (b -> b) -> [(a,b)] -> [(a,b)]
updateList a f lst = map (\(aa,b) -> (aa, if a == aa then f b else b)) lst

upTo :: Int -> [a] -> Maybe Text
upTo n cs = if length cs <= n then Nothing else Just $ pack ("Choose up to " ++ show n ++ " cards please.")

-- shuffling in the style you would do it with a regular deck of cards
shuffle :: (RandomGen r) => r -> [c] -> [c]
shuffle rg cards = snd $ foldr (.) id (replicate 7 shuffle') (rg,cards) where
  shuffle' :: (RandomGen r) => (r,[c]) -> (r,[c])
  shuffle' (r,[]) = (r, [])
  shuffle' (r,cs) = let (i, r') = randomR (1, length cs) r 
                        (start,end) = splitAt i cs
                        (r'',end') = shuffle' (r',end)
                    in (r'', end' ++ start)

