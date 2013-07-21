module Model.Card (

  copper, 
  silver, 
  gold,
  estate,
  duchy,
  province,
  curse,
  cellar,
  chapel,
  moat,
  chancellor,
  village,
  woodcutter,
  workshop,
  bureaucrat,
  feast,
  gardens,
  militia,
  moneylender,
  remodel,
  smithy,
  spy,
  thief,
  throneRoom
    
) where

import Model.GameState
import Model.GamePlay
import Model.Player
import Model.DominionGamePlay
import Control.Monad
import Prelude

moneyCard :: String -> Int -> Int -> Card
moneyCard n v m = card n v [Treasure (\_ -> increaseMoney m)]

copper :: Card
copper = moneyCard "Copper" 0 1
silver :: Card
silver = moneyCard "Silver" 2 2
gold :: Card
gold = moneyCard "Gold" 6 3

victory :: String -> Int -> Int -> Card
victory n c vp = card n c [Victory (\_ -> vp)]

estate :: Card
estate = victory "Estate" 2 1
duchy :: Card
duchy = victory "Duchy" 5 3
province :: Card
province = victory "Province" 8 6

curse :: Card
curse = card "Curse" 0 [Victory (\_ -> -1)]

noAction :: Player -> DominionGamePlay ()
noAction p = return ()

noPoints :: [Card] -> Int
noPoints cards = 0

-- 
-- action cards --
--

actionCard :: String -> Int -> (Player -> DominionGamePlay ()) -> Card
actionCard n v a = card n v [Action a]

attackCard :: String -> Int -> (Player -> DominionGamePlay ()) -> (Player -> Player -> DominionGamePlay ()) -> Card
attackCard n v ac at = card n v [Action ac, Attack at]

-- Cellar Action  $2  +1 Action
-- Discard any number of cards.
-- +1 Card per card discarded.
cellar :: Card
cellar = actionCard "Cellar" 2 cellarGamePlay
  where cellarGamePlay p = do
                            increaseActions 1
                            h <- hand p 
                            cards <- choose "Choose cards to discard" p h (\_ -> Nothing)
                            forM cards (\c -> discardCardFromHand c p)
                            forM cards (\c -> drawCardAndPutInHand p)
                            return ()  


-- Chapel Action  $2  Trash up to 4 cards from your hand.
chapel :: Card
chapel = actionCard "Chapel" 2 chapelGamePlay
  where chapelGamePlay p = do
                  h <- hand p
                  cards <- choose "Choose up to 4 cards to trash" p h (upTo 4)
                  forM cards (\c -> takeCardFromHand c p >> trashCard c)
                  return ()

-- Moat Action and Reaction $2  +2 Cards
-- When another player plays an Attack card, you may reveal this from your hand. If you do, you are unaffected by that Attack.
moat :: Card
moat = card "Moat" 2 [Reaction moatReaction, Action moatAction]
  where moatAction p = drawCardAndPutInHand p >> drawCardAndPutInHand p
        moatReaction _ = \p -> return ()

-- Chancellor Action  $3  +$2
-- You may immediately put your deck into your discard pile.
chancellor :: Card
chancellor = actionCard "Chancellor" 3 chancellorGamePlay
  where chancellorGamePlay p = do
          increaseMoney 3
          deckInDiscardPile <- decide "Put deck into discard pile?" p
          when deckInDiscardPile (putDeckOnDiscardPile p)
        putDeckOnDiscardPile p = do
          cnt <- drawSize p
          forM [1..cnt] (\_ -> drawAndDiscard p)
          return ()
        drawAndDiscard p = do
          mc <- drawCard p
          maybe (return ()) (\c -> discardCard c p) mc 


-- Village  Action  $3  +1 Card; +2 Actions.
village :: Card
village = actionCard "Village" 3 villageGamePlay
  where villageGamePlay p = (increaseActions 2) >> (drawCardAndPutInHand p)


-- Woodcutter Action  $3  +1 Buy; +$2.
woodcutter :: Card
woodcutter = actionCard "Woodcutter" 3 woodCutterGamePlay
  where woodCutterGamePlay p = (increaseBuys 1) >> (increaseMoney 2)


-- Workshop Action  $3  Gain a card costing up to $4.
workshop :: Card
workshop = actionCard "Workshop" 3 workshopGamePlay
  where workshopGamePlay p = do
          cs <- boardCards 4
          cards <- choose "Choose a card costing up to $4" p cs (exactly 1)
          takeCardFromBoard (head cards)
          discardCard (head cards) p

boardCards :: Int -> DominionGamePlay [Card]
boardCards n = do
  b <- currentBoard
  return (map fst $ filter (\p -> snd p <= n) b)

exactly :: Int -> [a] -> Maybe String
exactly n cs = if length cs == n then Nothing else Just $ "Choose exactly " ++ (show n) ++ " card(s) please."

-- Bureaucrat Action � Attack $4  Gain a silver card; put it on top of your deck. Each other player reveals a Victory card from his hand and puts it on his deck (or reveals a hand with no Victory cards).
bureaucrat :: Card
bureaucrat = attackCard "Bureaucrat" 4 bureaucratGamePlay bureaucratAttack
  where bureaucratGamePlay p = (takeCardFromBoard silver) >> (putCardOnTopOfDeck silver p)
        bureaucratAttack p victim = do
          h <- hand victim
          let victoryCardsInHand = filter isVictory h
          unless (null victoryCardsInHand) $ do
            vcs <- choose "Choose victory card to show" victim (filter isVictory h) (exactly 1)
            takeCardFromHand (head vcs) victim
            putCardOnTopOfDeck (head vcs) victim

-- Feast  Action  $4  Trash this card. Gain a card costing up to $5.
feast :: Card
feast = actionCard "Feast" 4 feastGamePlay
  where feastGamePlay p = do
          cs <- boardCards 5
          cards <- choose "Choose a card from board costing up to $5" p cs (exactly 1)
          takeCardFromBoard (head cards)
          discardCard (head cards) p
          takeCardFromTable feast >> trashCard feast
          return ()


--  Gardens Victory $4  Worth 1 Victory for every 10 cards in your deck (rounded down).
gardens :: Card
gardens = card "Gardens" 4 [Victory countPoints]
  where countPoints cards = (length cards) `div` 10
    

-- Militia  Action � Attack $4  +$2
-- Each other player discards down to 3 cards in his hand.
militia :: Card
militia = attackCard "Militia" 4 militiaAction militiaAttack
  where militiaAction _ = increaseMoney 2
        militiaAttack p victim = do
          currentHand <- hand victim
          unless (length currentHand <= 3) (discardDownToThree p victim)
        discardDownToThree p victim = do
          currentHand <- hand victim
          cards <- choose "Choose cards to discard (down to 3 in hand)" p currentHand (\cs -> if length currentHand == length cs + 3 then Nothing else Just "Discard until 3 cards left please")
          forM cards (\c -> discardCardFromHand c victim) 
          return ()
 
-- Moneylender  Action  $4  Trash a Copper  from your hand. If you do, +$3.
moneylender :: Card
moneylender = actionCard "Moneylender" 4 moneyLenderGamePlay
  where moneyLenderGamePlay p = do
          h <- hand p
          when (copper `elem` h) $ do
            trash <- decide "Trash a copper for $3?" p
            when trash $ do
              takeCardFromHand copper p
              trashCard copper
              increaseMoney 3

-- Remodel  Action  $4  Trash a card from your hand. Gain a card costing up to $2 more than the trashed card.
remodel :: Card
remodel = actionCard "Remodel" 4 remodelGamePlay
  where remodelGamePlay p = do
          currentHand <- hand p
          cards <- choose "Choose card to thrash" p currentHand (upTo 1)
          if cards == [] then return () else remodelCard (head cards) p
        remodelCard c p = do
          takeCardFromHand c p >> trashCard c
          cs <- boardCards (cardValue c + 2)
          cards <- choose "Choose card to gain" p cs (exactly 1)
          (takeCardFromBoard (head cards) >> discardCard (head cards) p)
  
-- Smithy Action  $4  +3 Cards.
smithy :: Card
smithy = actionCard "Smithy" 4 smithyGamePlay
  where smithyGamePlay p = drawCardAndPutInHand p >> drawCardAndPutInHand p >> drawCardAndPutInHand p

-- Spy  Action � Attack $4  +1 Card; +1 Action
-- Each player (including you) reveals the top card of his deck and either discards it or puts it back, your choice.
spy :: Card
spy = attackCard "Spy" 4 spyAction spyAttack
  where spyAction p = do
          increaseActions 1
          drawCardAndPutInHand p
          spyAttack p p
        spyAttack spy victim = do
          mc <- drawCard victim
          maybe (return ()) (spyOnCard spy victim) mc
        spyOnCard spy victim c = do
          putBack <- decide ("Put " ++ (show c) ++ " back on top of " ++ (show victim) ++ "'s deck?") spy
          (if putBack then putCardOnTopOfDeck else discardCard) c victim


-- Thief  Action � Attack $4  Each other player reveals the top 2 cards of his deck. If they revealed any Treasure cards, they trash one of them that you choose. You may gain any or all of these trashed cards. They discard the other revealed cards.
thief :: Card
thief = attackCard "Thief" 4 (\_ -> return ()) thiefAttack
  where thiefAttack thief victim = do
          cs <- drawCards victim 2
          toTrash <- choose ("Choose card to steal from " ++ show (victim)) thief (filter isTreasure cs) (upTo 1)
          forM toTrash (gainOrTrash thief)
          return ()
        gainOrTrash thief c = do
          gain <- decide ("Do you want to gain a " ++ (show c) ++ " ?") thief
          if gain then discardCard c thief else trashCard c

          
-- Throne Room  Action  $4  Choose an Action card in your hand. Play it twice.
throneRoom :: Card
throneRoom = actionCard "Throne Room" 4 throneRoomGamePlay
  where throneRoomGamePlay p = do
          currentHand <- hand p
          cards <- choose "Choose action to play twice" p (filter isAction currentHand) (upTo 1)
          unless (null cards) $ playTwice (head cards) p
        playTwice c p = do
          putCardOnTable c
          cardGamePlay c p
          cardGamePlay c p
          
-- Council Room Action  $5  +4 Cards; +1 Buy
-- Each other player draws a card.
councilRoom :: Card
councilRoom = actionCard "Council Room" 5 playCouncilRoom
  where playCouncilRoom p = do
          increaseBuys 1
          drawCardAndPutInHand p
          drawCardAndPutInHand p
          drawCardAndPutInHand p
          drawCardAndPutInHand p
          otherPlayers <- fmap tail allPlayers
          forM otherPlayers drawCardAndPutInHand
          return ()

-- Festival Action  $5  +2 Actions, +1 Buy; +$2.
festival :: Card
festival = actionCard "Festival" 5 playFestival
  where playFestival p = do
          increaseActions 2
          increaseBuys 1
          increaseMoney 2

-- Laboratory Action  $5  +2 Cards; +1 Action.
laboratory :: Card
laboratory = actionCard "Laboratory" 5 playLab
  where playLab p = do
          drawCardAndPutInHand p
          drawCardAndPutInHand p
          increaseActions 1

-- Library  Action  $5  Draw until you have 7 cards in hand. 
--   You may set aside any Action cards drawn this way, as you draw them; 
--   discard the set aside cards after you finish drawing.
library :: Card
library = actionCard "Library" 5 playLib
  where playLib p = do
          h <- hand p
          unless (length h >= 7) $ do
            mc <- drawCard p
            maybe (return ()) (decideOnCard p) mc
        decideOnCard p c = do
          sa <- use p c
          if sa then (putCardInHand c p >> playLib p) else (playLib p >> discardCard c p)
        use p c = 
          if isAction c 
            then decide ("Put " ++ show c ++ " in hand?") p
            else return True
                  

-- Market Action  $5  +1 Card; +1 Action; +1 Buy; +$1.
market :: Card
market = actionCard "Market" 5 playMarket
  where playMarket p = drawCard p >> increaseActions 1 >> increaseBuys 1 >> increaseMoney 1

-- Mine Action  $5  Trash a Treasure card from your hand. Gain a Treasure card costing up to $3 more; put it into your hand.
mine :: Card
mine = actionCard "Mine" 5 playMine
  where playMine p = do
          h <- hand p
          treasureCard <- choose "Choose treasure to trash" p (filter isTreasure h) (upTo 1)
          unless (null treasureCard) $ do
            trashCard (head treasureCard)
            b <- currentBoard
            let treasures = map fst $ filter (\(c,n) -> n > 0 && isTreasure c && cardValue c <= 3 + (cardValue (head treasureCard))) b 
            newTreasure <- choose "Choose treasure to gain" p treasures (exactly 1)
            putCardInHand (head newTreasure) p

-- Witch  Action � Attack $5  +2 Cards
-- Each other player gains a Curse card.
witch :: Card
witch = attackCard "Witch" 5 witchAction witchAttack
  where witchAction p = drawCardAndPutInHand p >> drawCardAndPutInHand p
        witchAttack _ p = do
          b <- currentBoard
          unless (getFromList curse b == 0) $ do
            takeCardFromBoard curse
            discardCard curse p

-- Adventurer Action  $6  Reveal cards from your deck until you reveal 2 Treasure cards. 
-- Put those Treasure cards in your hand and discard the other revealed cards.
adventurer :: Card
adventurer = actionCard "Adventurer" 6 (adventurerAction 2)
  where adventurerAction 0 _ = return ()
        adventurerAction n p = do
          mc <- drawCard p
          maybe (return ()) (withCard n p) mc
        withCard n p c = do
          if isTreasure c 
            then (adventurerAction (n-1) p) >> (putCardInHand c p)
            else (adventurerAction n p) >> (discardCard c p)
 