data GameState = 
  WaitingForPlayers Int | 
  Finished | 
  Playing { steps :: [Step], tableState :: TableState,  } deriving (Generic)

data PlayData = PlayData { 
  actions :: Int,
  money :: Int,
  buy :: Int } deriving (Generic)

initialTableState :: PlayData
initialTableState = PlayData [] 1 0 1
     
data Step = 
  UserInteraction String Int UserInput |
  Immediate StateChange

data UserInput = 
  CardInput ([Card] -> Either String StateChange) |
  DecisionInput (Bool -> Either String StateChange)
  
type StateChange = GameState -> GameState

aggregate :: [Step] -> Step
aggregate [] = Immediate id
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
    ci' cards gs = ci cards gs >>= \gs -> pushStep s gs   
  (UserInteraction msg p (DecisionInput di)) <.> s2 = UserInteraction msg p (DecisionInput di') where
    di' decision b ps = di decision b ps >>= \(steps,b',ps') -> Right (steps++[s2],b',ps')   
  (Immediate s1) <.> s2 = Immediate s' where
    s' b ps = s1 b ps >>= (\(steps,b',ps') -> Right (steps++[s2],b',ps'))

---
--- basic steps
---
cardStep :: String -> Player -> ([Card] -> Step) -> Step
cardStep msg p sf = UserInteraction msg p (CardInput css)
  where css cards b ps = Right ([sf cards],b,ps)

cardsStep :: (Card -> Step) -> [Card] -> Step
cardsStep cs cards = aggregate (map cs cards) 

eachOtherPlayer :: Player -> (Player -> Step) -> Step
eachOtherPlayer p sf = Immediate sc
  where sc b ps = Right (map sf [0 .. length ps - 1], b, ps)

handStep :: Player -> ([Card] -> Step) -> Step
handStep p sf = Immediate (handStepStateChange p)
  where handStepStateChange p b ps = Right ([s'],b,ps) 
          where s' = sf (hand (playerStates $ ps!p))

decisionStep :: String -> Player -> (Bool -> Step) -> Step
decisionStep msg p sf = UserInteraction msg p (DecisionInput dss)
  where dss d b ps = Right ([sf d],b,ps)

errorStep :: String -> Step
errorStep msg = Immediate (\_ _ -> Left msg)

idStep :: Step
idStep = Immediate (\b ps -> Right ([], b, ps))

simpleStep :: Player -> (PlayerState -> Either String PlayerState) -> Step
simpleStep p psf = Immediate $ simpleStateChange psf p

-- takes one card from the drawing pile (potentially reshuffles) and puts it into the player's hand
drawCard :: Player -> Step
drawCard p = simpleStep p (\ps -> Right $ drawCard' ps)

-- takes one card from the player's hand puts it into the discard pile
discardCard :: Player -> Card -> Step
discardCard p c = simpleStep p (discardCard' c)

-- takes one card from the player's hand and trashes it
trashCard :: Player -> Card -> Step
trashCard p c = simpleStep p (trashCard' c)

changeActions :: Player -> Int -> Step
changeActions p i = simpleStep p (updateAction i)

changeBuy :: Player -> Int -> Step
changeBuy p i = simpleStep p (updateBuy i)

changeMoney :: Player -> Int -> Step
changeMoney p i = simpleStep p (updateMoney i)

-- takes one card from the board and puts it into the discard pile
takeCardFromBoard :: Player -> Card -> Step
takeCardFromBoard p c = Immediate sc where
  sc b ps = do
    b' <- takeFromBoard c b
    ps' <- updatePlayer (addToDiscard c) p ps
    return ([] :: [Step], b', ps')
  
putDeckOnDiscardPile :: Player -> Step
putDeckOnDiscardPile p = simpleStep p discardDeck
  where discardDeck ps = Right $ PlayerState { hand = hand ps, draw = [], discard = draw ps ++ discard ps,
    table = table ps, actions = actions ps, money = money ps, buy = buy ps }

takeCardFromTable :: Player -> Card -> Step
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

gotoNextPlayer :: Player -> Step
gotoNextPlayer p = Immediate stateChange
  where stateChange b ps = 
          let nextPlayer = if p+1 < length ps then p+1 else 0
          in Right ([playTurn nextPlayer],b,ps) 

playTurn :: Player -> Step
playTurn p = Immediate stateChange
  where stateChange b ps = Right ([playActions p,buyCards p,cleanUp p,checkEndOfGame p],b,ps)

playActions :: Player -> Step
playActions p = UserInteraction "Choose action card to play" p (CardInput $ playActions' p)

playActions' :: Player -> [Card] -> StateChange
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

buyCards :: Player -> Step
buyCards p = UserInteraction "Choose the cards you want to buy" p (CardInput buyCards')
  where buyCards' cards b ps = Right (map (\c -> Immediate (buyCard' c)) cards,b,ps)
        buyCard' c b ps = do
          b' <- takeFromBoard c b
          ps' <- updatePlayer (buyCard c) p ps
          return ([] :: [Step], b', ps')

cleanUp :: Player -> Step
cleanUp p = simpleStep p cleanUpPlayer
  
checkEndOfGame :: Player -> Step
checkEndOfGame p = Immediate stateChange
  where hasEnded b = length (filter (\(_,n) -> n == 0) b) >= 3 
        stateChange b ps = Right (if hasEnded b then [] else [gotoNextPlayer p], b, ps) 

simpleStateChange :: (PlayerState -> Either String PlayerState) -> Player -> StateChange
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

updatePlayer :: (PlayerState -> Either String PlayerState) -> Player -> [Player] -> Either String [Player]
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
                        
getFromList :: (Eq a) => a -> [(a,b)] -> b
getFromList a ((aa,bb):abs) = if a == aa then bb else getFromList a abs

updateList :: (Eq a) => a -> (b -> b) -> [(a,b)] -> [(a,b)]
updateList a f abs = map (\(aa,bb) -> (aa, if a == aa then f b else b)) abs







---------------------------------------------------------------------------------------






dominionCards :: [Card]
dominionCards = [cellar, chapel, moat, chancellor, village, woodcutter, workshop, bureaucrat,
  feast, gardens, militia, moneylender, remodel, smithy, spy, thief, throneRoom, 
  councilRoom, festival, laboratory, library, market, mine, witch, adventurer]

initialDeck :: [Card] 
initialDeck = [ copper, copper, copper, copper, copper, copper, copper, estate, estate, estate ]

initialBoard :: Int -> Board
initialBoard n = let victories = if n == 2 then 8 else 12
  in zip (take 10 dominionCards) (repeat 10) ++
       [ (copper, 50), (silver, 50), (gold, 50), 
         (estate, victories), (duchy, victories), (province, victories),
     (curse, (n - 1) * 10) ]  
  
noAction :: PlayerRef -> Step
noAction _ = errorStep "This is not an action card"

-- Int -> Board -> [Player] -> Either String ([Step], Board, [Player])

--
-- actions
--

action :: String -> Int -> (PlayerRef -> Step) -> Card
action n c ap = card n c (\_ -> 0) ap

chooseCardsAction :: String -> Int -> String -> (PlayerRef -> [Card] -> Step) -> Card
chooseCardsAction n c msg ap = action n c playCard
  where playCard p = cardStep msg p (ap p)

choiceAction :: String -> Int -> String -> (PlayerRef -> Bool -> Step) -> Card
choiceAction n c msg ap = action n c playCard
  where playCard p = decisionStep msg p (ap p)
 
 

throneRoom :: Card
throneRoom = chooseCardsAction "Throne Room" 4 playThroneRoom
  playThroneRom p ([c]) = (cardActionPlay c p) <.> (cardActionPlay c p)
-- Throne Room  Action  $4  Choose an Action card in your hand. Play it twice.

councilRoom :: Card
councilRoom = action "Council Room" 5 playCouncilRoom
  where playCouncilRoom p = (updateBuy p 1) <.> (drawCard p) <.> (drawCard p) <.> (drawCard p) <.> (drawCard p) <.> eachOtherPlayer p drawCard
-- Council Room Action  $5  +4 Cards; +1 Buy
-- Each other player draws a card.

festival :: Card
festival = action "Festival" 5 playFestival
  where playFestival p = updateBuy p 1 <.> updateMoney p 2 <.> updateAction p 2
-- Festival Action  $5  +2 Actions, +1 Buy; +$2.

laboratory :: Card
laboratory = action "Laboratory" 5 playLab
  where playLab p = drawCard p <.> drawCard p <.> updateAction p 1
-- Laboratory Action  $5  +2 Cards; +1 Action.

library :: Card
library = undefined
-- Library  Action  $5  Draw until you have 7 cards in hand. You may set aside any Action cards drawn this way, as you draw them; discard the set aside cards after you finish drawing.

market :: Card
market = action "Market" 5 playMarket
  where playMarket p = drawCard p <.> updateAction p 1 <.> updateBuy p 1 <.> updateMoney p 1
-- Market Action  $5  +1 Card; +1 Action; +1 Buy; +$1.

mine :: Card
mine = undefined
-- Mine Action  $5  Trash a Treasure card from your hand. Gain a Treasure card costing up to $3 more; put it into your hand.

witch :: Card
witch = action "Witch" 5 playWitch
  where playWitch p = drawCard p <.> drawCard p <.> eachOtherPlayer p takeCurse
        takeCurse p b ps = case takeCardFromBoard p curse b ps of
          Right t => Right t
          _ => Right ([], b, ps) 
-- Witch  Action � Attack $5  +2 Cards
-- Each other player gains a Curse card.

adventurer :: Card
adventurer = undefined
-- Adventurer Action  $6  Reveal cards from your deck until you reveal 2 Treasure cards. Put those Treasure cards in your hand and discard the other revealed cards.
