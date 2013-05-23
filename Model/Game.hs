{-# LANGUAGE DeriveGeneric,TypeSynonymInstances,FlexibleInstances #-}
module Model.Game (

  Card,
  cardValue,
  Board,
  PlayerRef,
  Step,
  errorStep,
  idStep,
  cardStep,
  cardsStep,
  decisionStep,
  handStep,
  card,
  discardCard,
  drawCard,
  trashCard,
  changeMoney,
  changeActions,
  changeBuy,
  eachOtherPlayer,
  putDeckOnDiscardPile,
  takeCardFromBoard,
  takeCardFromTable,
  Combinator((<.>))
  
) where

import Prelude
import Data.Text (Text, unpack)
import GHC.Generics
import Data.Aeson as Aeson
import Control.Monad.Error

--
-- Card
--
data Card = Card { 
  cardName :: String, 
  cardValue :: Int, 
  victoryPoints :: Game -> Int, 
  cardActionPlay :: PlayerRef -> Step }

card :: String -> Int -> (Game -> Int)  -> (PlayerRef -> Step) -> Card
card = Card

instance Show Card where                                                                                       
    show c = cardName c                                                                            

instance Eq Card where
  c1 == c2 = (cardName c1) == (cardName c2)
      
type Board = [(Card, Int)]

data Game = Game { gameState :: GameState, board :: Board, players :: [Player] } deriving (Generic)

data Player = Player { name :: Text, playerState :: PlayerState  } deriving (Generic)

instance Eq Player where
  p1 == p2 = (name p1) == (name p2)
  
data PlayerState = PlayerState { 
  hand :: [Card], 
  draw :: [Card], 
  discard :: [Card],
  table :: [Card],
  actions :: Int,
  money :: Int,
  buy :: Int } deriving (Generic)

data GameState = 
  WaitingForPlayers Int | 
  Finished | 
  Playing { steps :: [Step] } deriving (Generic)

data Step = 
  UserInteraction String Int UserInput |
  Immediate StateChange

data UserInput = 
  CardInput ([Card] -> StateChange) |
  DecisionInput (Bool -> StateChange)
  
type StateChange = Board -> [Player] -> Either String ([Step],Board,[Player])

aggregate :: [Step] -> Step
aggregate [] = Immediate (\b ps -> Right([],b,ps))
aggregate (s:steps) = s <.> aggregate steps

--- 
--- Combinator
---
class Combinator a where
  (<.>) :: a -> a -> a

instance (Combinator a) => Combinator (b -> a) where
  f <.> g = \b -> (f b) <.> (g b) 

instance Combinator Step where
  (UserInteraction msg p (CardInput ci)) <.> s2 = UserInteraction msg p (CardInput ci') where
    ci' cards b ps = ci cards b ps >>= \(steps,b',ps') -> Right (steps++[s2],b',ps')   
  (UserInteraction msg p (DecisionInput di)) <.> s2 = UserInteraction msg p (DecisionInput di') where
    di' decision b ps = di decision b ps >>= \(steps,b',ps') -> Right (steps++[s2],b',ps')   
  (Immediate s1) <.> s2 = Immediate s' where
    s' b ps = s1 b ps >>= (\(steps,b',ps') -> Right (steps++[s2],b',ps'))

---
--- basic steps
---
type PlayerRef = Int

cardStep :: String -> PlayerRef -> ([Card] -> Step) -> Step
cardStep msg p sf = UserInteraction msg p (CardInput css)
  where css cards b ps = Right ([sf cards],b,ps)

cardsStep :: (Card -> Step) -> [Card] -> Step
cardsStep cs cards = aggregate (map cs cards) 

eachOtherPlayer :: PlayerRef -> (PlayerRef -> Step) -> Step
eachOtherPlayer p sf = Immediate sc
  where sc b ps = Right (map sf [0 .. length ps - 1], b, ps)

handStep :: PlayerRef -> ([Card] -> Step) -> Step
handStep p sf = Immediate (handStepStateChange p)
  where handStepStateChange p b ps = Right ([s'],b,ps) 
          where s' = sf (hand (playerState $ ps!!p))

decisionStep :: String -> PlayerRef -> (Bool -> Step) -> Step
decisionStep msg p sf = UserInteraction msg p (DecisionInput dss)
  where dss d b ps = Right ([sf d],b,ps)

errorStep :: String -> Step
errorStep msg = Immediate (\_ _ -> Left msg)

idStep :: Step
idStep = Immediate (\b ps -> Right ([], b, ps))

simpleStep :: PlayerRef -> (PlayerState -> Either String PlayerState) -> Step
simpleStep p psf = Immediate $ simpleStateChange psf p

-- takes one card from the drawing pile (potentially reshuffles) and puts it into the player's hand
drawCard :: PlayerRef -> Step
drawCard p = simpleStep p (\ps -> Right $ drawCard' ps)

-- takes one card from the player's hand puts it into the discard pile
discardCard :: PlayerRef -> Card -> Step
discardCard p c = simpleStep p (discardCard' c)

-- takes one card from the player's hand and trashes it
trashCard :: PlayerRef -> Card -> Step
trashCard p c = simpleStep p (trashCard' c)

changeActions :: PlayerRef -> Int -> Step
changeActions p i = simpleStep p (updateAction i)

changeBuy :: PlayerRef -> Int -> Step
changeBuy p i = simpleStep p (updateBuy i)

changeMoney :: PlayerRef -> Int -> Step
changeMoney p i = simpleStep p (updateMoney i)

-- takes one card from the board and puts it into the discard pile
takeCardFromBoard :: PlayerRef -> Card -> Step
takeCardFromBoard p c = Immediate sc where
  sc b ps = do
    b' <- takeFromBoard c b
    ps' <- updatePlayer (addToDiscard c) p ps
    return ([] :: [Step], b', ps')
  
putDeckOnDiscardPile :: PlayerRef -> Step
putDeckOnDiscardPile p = simpleStep p discardDeck
  where discardDeck ps = Right $ PlayerState { hand = hand ps, draw = [], discard = draw ps ++ discard ps,
    table = table ps, actions = actions ps, money = money ps, buy = buy ps }

takeCardFromTable :: PlayerRef -> Card -> Step
takeCardFromTable p c = simpleStep p takeFromTable
  where takeFromTable ps = do
          table' <- without c $ table ps
          return PlayerState { hand = hand ps, draw = draw ps, discard = discard ps,
                         table = table', actions = actions ps, money = money ps, buy = buy ps }

--
-- advancing the game
--
chooseCards :: [Card] -> Game -> Either String Game
chooseCards cards (Game (Playing ((UserInteraction _ _ (CardInput cont)):steps)) b ps) = applyStateChange (cont cards) steps b ps
chooseCards _ _ = Left "Invalid play"

decide :: Bool -> Game -> Either String Game
decide choice (Game (Playing ((UserInteraction _ _ (DecisionInput cont)):steps)) b ps) = applyStateChange (cont choice) steps b ps
decide _ _ = Left "Invalid play"

applyStateChange :: StateChange -> [Step] -> Board -> [Player] -> Either String Game
applyStateChange sc s b ps = do
  (additionalSteps, b', ps') <- sc b ps
  return $ Game (Playing $ additionalSteps ++ s) b' ps'
  
continue :: Game -> Either String Game
continue = undefined

--
--

gotoNextPlayer :: PlayerRef -> Step
gotoNextPlayer p = Immediate stateChange
  where stateChange b ps = 
          let nextPlayer = if p+1 < length ps then p+1 else 0
          in Right ([playTurn nextPlayer],b,ps) 

playTurn :: PlayerRef -> Step
playTurn p = Immediate stateChange
  where stateChange b ps = Right ([playActions p,buyCards p,cleanUp p,checkEndOfGame p],b,ps)

playActions :: PlayerRef -> Step
playActions p = UserInteraction "Choose action card to play" p (CardInput $ playActions' p)

playActions' :: PlayerRef -> [Card] -> StateChange
playActions' _ [] = \b ps -> Right ([], b, ps)
playActions' p (c:[]) = \b ps -> Right ([simpleStep p (prepare c), cardActionPlay c p, playActions p], b, ps)
  where prepare c ps = takeFromHand c ps >>= putOnTable c >>= updateAction (-1)
        putOnTable c = \ps -> Right $ PlayerState { hand = hand ps, draw = draw ps, 
                                                discard = discard ps, table = c : (table ps),
                                                actions = actions ps, 
                                                money = money ps, buy = buy ps }

  
playActions' _ _ = \b ps -> Left "Please choose a single action"

--  where playActions' [] = \b ps -> Right ([], b, ps)
--        playActions' (c:[]) = (cardActionPlay c) p
--        playActions' _ = \b ps -> Left "Please choose a single action"

-- Board -> [Player] -> Either String ([Step],Board,[Player])

buyCards :: PlayerRef -> Step
buyCards p = UserInteraction "Choose the cards you want to buy" p (CardInput buyCards')
  where buyCards' cards b ps = Right (map (\c -> Immediate (buyCard' c)) cards,b,ps)
        buyCard' c b ps = do
          b' <- takeFromBoard c b
          ps' <- updatePlayer (buyCard c) p ps
          return ([] :: [Step], b', ps')

cleanUp :: PlayerRef -> Step
cleanUp p = simpleStep p cleanUpPlayer
  
checkEndOfGame :: PlayerRef -> Step
checkEndOfGame p = Immediate stateChange
  where hasEnded b = length (filter (\(_,n) -> n == 0) b) >= 3 
        stateChange b ps = Right (if hasEnded b then [] else [gotoNextPlayer p], b, ps) 

simpleStateChange :: (PlayerState -> Either String PlayerState) -> PlayerRef -> StateChange
simpleStateChange up p b ps = do
  ps' <- updatePlayer up p ps
  return ([], b, ps')

--
-- Json
--
instance Aeson.ToJSON Game
instance Aeson.ToJSON Player
instance Aeson.ToJSON GameState
instance Aeson.ToJSON Step where
  toJSON (UserInteraction msg p _) = toJSON $ "Waiting for player " ++ (show p) ++ ": '" ++ msg ++ "'"
  toJSON (Immediate _) = toJSON "Immediate"

instance Aeson.ToJSON PlayerState

instance Aeson.ToJSON Card where
  toJSON c = toJSON $ show c

  
newGame :: Int -> Board -> Game
newGame n b = Game (WaitingForPlayers n) b []

---
--- WaitingForPlayer 
---
join :: Text -> [Card] -> Game -> Either String Game
join p initialDeck g = case gameState g of
  WaitingForPlayers 1 -> Right $ Game {
    gameState = Playing [playTurn 0],
    board = board g,
    players = (Player p $ initialPlayerState initialDeck):(players g) }
  WaitingForPlayers n -> Right $ Game {
    gameState = WaitingForPlayers (n-1),
    board = board g,
    players = (Player p $ initialPlayerState initialDeck):(players g) }
  _ -> Left "Can't join -- game in progress"

---
--- Player steps
---

takeFromBoard :: Card -> Board -> Either String Board
takeFromBoard c1 ((c2,n):b) = if c1 == c2 
  then if n > 0 then Right $ (c1,n-1):b else Left $ (cardName c1) ++ " is not available"
  else fmap ((c2,n):) (takeFromBoard c1 b)

takeFromHand :: Card -> PlayerState -> Either String PlayerState
takeFromHand c ps = do
  hand' <- without c $ hand ps
  return PlayerState { hand = hand', draw = draw ps, discard = discard ps, table = table ps, actions = actions ps, money = money ps, buy = buy ps }

buyCard :: Card -> PlayerState -> Either String PlayerState
buyCard c ps = updateBuy (-1) ps >>= updateMoney (- (cardValue c)) >>= addToDiscard c

updatePlayer :: (PlayerState -> Either String PlayerState) -> PlayerRef -> [Player] -> Either String [Player]
updatePlayer u 0 (p:ps) = do
  state <- u $ playerState p
  return $ (Player (name p) state):ps
updatePlayer u n (p:ps) = do
  ps' <- updatePlayer u (n-1) ps
  return $ p:ps'
 
updateAction :: Int -> PlayerState -> Either String PlayerState
updateAction n ps = Right PlayerState { hand = hand ps, draw = draw ps, discard = discard ps,
  table = table ps, actions = n + actions ps, money = money ps, buy = buy ps }

takeFirst :: (Eq a) => a -> [a] -> [a]
takeFirst c1 (c2:cs) = if c1 == c2 then cs else c1 : takeFirst c1 cs
 
updateBuy :: Int -> PlayerState -> Either String PlayerState
updateBuy n ps = if buy ps + n >= 0 
    then Right PlayerState { hand = hand ps, draw = draw ps, discard = discard ps,
      table = table ps, actions = actions ps, money = money ps, buy = n + buy ps }
    else Left "No buys left"

updateMoney :: Int -> PlayerState -> Either String PlayerState
updateMoney n ps = if money ps + n >= 0
  then Right PlayerState { hand = hand ps, draw = draw ps, discard = discard ps,
    table = table ps, actions = actions ps, money = n + money ps, buy = buy ps }
  else Left "Not enough money"
  
addToDiscard :: Card -> PlayerState -> Either String PlayerState
addToDiscard c ps = Right $ PlayerState { hand = hand ps, draw = draw ps, discard = c : (discard ps),
  table = table ps, actions = actions ps, money = money ps, buy = buy ps }

addToHand :: Card -> PlayerState -> Either String PlayerState
addToHand c ps = Right $ PlayerState { hand = c : (hand ps), draw = draw ps, discard = discard ps,
  table = table ps, actions = actions ps, money = money ps, buy = buy ps }
  
cleanUpPlayer :: PlayerState -> Either String PlayerState
cleanUpPlayer ps = Right $ drawCard' . drawCard' . drawCard' . drawCard' . drawCard' $ 
  PlayerState { hand = [], 
    draw = draw ps, 
    discard = (hand ps) ++ (table ps) ++ (discard ps),
    table = [],
    actions = 1,
    money = 0,
    buy = 1 } 
    
initialPlayerState :: [Card] -> PlayerState
initialPlayerState initialDeck = drawCard' . drawCard' . drawCard' . drawCard' . drawCard' $ 
  PlayerState { hand = [], discard = initialDeck, draw = [],
    table = [], actions = 1, money = 0, buy = 1 }

discardCard' :: Card -> PlayerState -> Either String PlayerState
discardCard' c ps = do
  hand' <- without c $ hand ps
  return PlayerState { hand = hand', draw = draw ps, discard = c:(discard ps), 
    table = table ps, actions = actions ps, money = money ps, buy = buy ps }

trashCard' :: Card -> PlayerState -> Either String PlayerState
trashCard' c ps = do
  hand' <- without c $ hand ps
  return PlayerState { hand = hand', draw = draw ps, discard = discard ps, 
    table = table ps, actions = actions ps, money = money ps, buy = buy ps }
  
-- todo add shuffle  
drawCard' :: PlayerState -> PlayerState
drawCard' ps = case (draw ps) of
  c:cs -> PlayerState { hand = c:(hand ps), draw = cs, discard = discard ps, table = table ps, 
                        actions = actions ps, money = money ps, buy = buy ps }
  [] -> case (discard ps) of
      [] -> ps
      c:cs -> PlayerState { hand = c:(hand ps), draw = cs, discard = [], table = table ps,
                            actions = actions ps, money = money ps, buy = buy ps }



without :: (Eq a) => a -> [a] -> Either String [a]
without _ [] = Left "No such card"
without c1 (c2:cs) = if c1 == c2 then Right cs else do
                        cs' <- without c1 cs
                        return $ c2:cs'