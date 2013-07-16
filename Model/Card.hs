module Model.Card(copper, silver, gold) 

where

import Model.DominionState
import Model.Dominion
import Control.Monad
import Prelude

moneyCard :: String -> Int -> Card
moneyCard n v = card n v noPoints noAction

copper :: Card
copper = moneyCard "Copper" 0
silver :: Card
silver = moneyCard "Silver" 2
gold :: Card
gold = moneyCard "Gold" 6

victory :: String -> Int -> Int -> Card
victory n c vp = card n c (\_ -> vp) noAction

estate :: Card
estate = victory "Estate" 2 1
duchy :: Card
duchy = victory "Duchy" 5 3
province :: Card
province = victory "Province" 8 6

curse :: Card
curse = card "Curse" 0 (return (-1)) noAction

noAction :: Player -> GamePlay ()
noAction p = return ()

noPoints :: [Card] -> Int
noPoints cards = 0

-- 
-- action cards --
--

-- Cellar Action  $2  +1 Action
-- Discard any number of cards.
-- +1 Card per card discarded.
cellar :: Card
cellar = card "Cellar" 2 noPoints cellarGamePlay
  where cellarGamePlay p = do
                            increaseActions 1
                            h <- hand p 
                            cards <- chooseCards "Choose cards to discard" p h (\_ -> Nothing)
                            forM cards (\c -> discardCardFromHand c p)
                            forM cards (\c -> drawCardAndPutInHand p)
                            return ()  


-- Chapel Action  $2  Trash up to 4 cards from your hand.
chapel :: Card
chapel = card "Chapel" 2 noPoints chapelGamePlay
  where chapelGamePlay p = do
                  h <- hand p
                  cards <- chooseCards "Choose cards to trash" p h (upTo 4)
                  forM cards (\c -> discardCardFromHand c p >> trashCard c)
                  return ()

-- Moat Action and Reaction $2  +2 Cards
-- When another player plays an Attack card, you may reveal this from your hand. If you do, you are unaffected by that Attack.
moat :: Card
moat = card "Moat" 2 noPoints moatGamePlay
  where moatGamePlay p = (drawCardAndPutInHand p) >> (drawCardAndPutInHand p)

-- Chancellor Action  $3  +$2
-- You may immediately put your deck into your discard pile.
chancellor :: Card
chancellor = card "Chancellor" 3 noPoints chancellorGamePlay
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
village = card "Village" 3 noPoints villageGamePlay
  where villageGamePlay p = (increaseActions 2) >> (drawCardAndPutInHand p)


-- Woodcutter Action  $3  +1 Buy; +$2.
woodcutter :: Card
woodcutter = card "Woodcutter" 3 noPoints woodCutterGamePlay
  where woodCutterGamePlay p = (increaseBuys 1) >> (increaseMoney 2)


-- Workshop Action  $3  Gain a card costing up to $4.
workshop :: Card
workshop = card "Workshop" 3 noPoints workshopGamePlay
  where workshopGamePlay p = do
          cs <- boardCards 4
          cards <- chooseCards "Choose a card costing up to $4" p cs (exactly 1)
          takeCardFromBoard (head cards)
          discardCard (head cards) p

boardCards :: Int -> GamePlay [Card]
boardCards n = do
  b <- currentBoard
  return (map fst $ filter (\p -> snd p <= n) b)

exactly :: Int -> [a] -> Maybe String
exactly n cs = if length cs == n then Nothing else Just $ "Choose exactly " ++ (show n) ++ " card(s) please."

-- Bureaucrat Action � Attack $4  Gain a silver card; put it on top of your deck. Each other player reveals a Victory card from his hand and puts it on his deck (or reveals a hand with no Victory cards).
bureaucrat :: Card
bureaucrat = card "Bureaucrat" 4 noPoints bureaucratGamePlay
  where bureaucratGamePlay p = do
          (takeCardFromBoard silver) >> (putCardOnTopOfDeck silver p)
          attack undefined
          return ()

-- Feast  Action  $4  Trash this card. Gain a card costing up to $5.
feast :: Card
feast = card "Feast" 4 noPoints feastGamePlay
  where feastGamePlay p = do
          cs <- boardCards 5
          cards <- chooseCards "Choose a card from board costing up to $5" p cs (exactly 1)
          takeCardFromBoard (head cards)
          discardCard (head cards) p
          takeCardFromTable feast >> trashCard feast
          return ()


--  Gardens Victory $4  Worth 1 Victory for every 10 cards in your deck (rounded down).
gardens :: Card
gardens = card "Gardens" 4 countPoints noAction
  where countPoints cards = (length cards) `div` 10
    

-- Militia  Action � Attack $4  +$2
-- Each other player discards down to 3 cards in his hand.
militia :: Card
militia = card "Militia" 4 noPoints militiaGamePlay
  where militiaGamePlay p = do
          increaseMoney 2
          attack (militiaAttack p)
          return ()
        militiaAttack p victim = do
          currentHand <- hand victim
          if length currentHand <= 3 then return () else discardDownToThree p victim
        discardDownToThree p victim = do
          currentHand <- hand victim
          cards <- chooseCards "Choose cards to discard (down to 3 in hand)" p currentHand (\cs -> if length currentHand == length cs + 3 then Nothing else Just "Discard until 3 cards left please")
          forM cards (\c -> discardCardFromHand c victim) 
          return ()
 
-- Moneylender  Action  $4  Trash a Copper  from your hand. If you do, +$3.
moneylender :: Card
moneylender = card "Moneylender" 4 noPoints moneyLenderGamePlay
  where moneyLenderGamePlay p = do
          -- todo check card contains copper
          trash <- decide "Trash a copper for $3?" p
          if trash then burnCopper p else return ()
        burnCopper p = do
          (takeCardFromHand copper p)
          trashCard copper
          increaseMoney 3
          return ()   

-- Remodel  Action  $4  Trash a card from your hand. Gain a card costing up to $2 more than the trashed card.
remodel :: Card
remodel = card "Remodel" 4 noPoints remodelGamePlay
  where remodelGamePlay p = do
          currentHand <- hand p
          cards <- chooseCards "Choose card to thrash" p currentHand (upTo 1)
          if cards == [] then return () else remodelCard (head cards) p
        remodelCard c p = do
          takeCardFromHand c p >> trashCard c
          cs <- boardCards (cardValue c + 2)
          cards <- chooseCards "Choose card to gain" p cs (exactly 1)
          (takeCardFromBoard (head cards) >> discardCard (head cards) p)
  
-- Smithy Action  $4  +3 Cards.
smithy :: Card
smithy = card "Smithy" 4 noPoints smithyGamePlay
  where smithyGamePlay p = drawCardAndPutInHand p >> drawCardAndPutInHand p >> drawCardAndPutInHand p

-- Spy  Action � Attack $4  +1 Card; +1 Action
-- Each player (including you) reveals the top card of his deck and either discards it or puts it back, your choice.
spy :: Card
spy = card "Spy" 4 noPoints spyGamePlay
  where spyGamePlay p = do
          increaseActions 1
          drawCardAndPutInHand p
          spyAction p p
          attack (spyAction p)
          return ()
        spyAction spy victim = do
          mc <- drawCard victim
          maybe (return ()) (spyOnCard spy victim) mc
        spyOnCard spy victim c = do
          putBack <- decide ("Put " ++ (show c) ++ " back on top of " ++ (show victim) ++ "'s deck?") spy
          (if putBack then putCardOnTopOfDeck else discardCard) c victim


-- Thief  Action � Attack $4  Each other player reveals the top 2 cards of his deck. If they revealed any Treasure cards, they trash one of them that you choose. You may gain any or all of these trashed cards. They discard the other revealed cards.
thief :: Card
thief = card "Thief" 4 noPoints thiefGamePlay
  where thiefGamePlay p = do
          attack (thiefAction p)
          return ()
        thiefAction thief victim = do
          cs <- drawCards victim 2
          -- todo filter treasures
          toTrash <- chooseCards ("Choose card to steal from " ++ show (victim)) thief cs (upTo 1)
          forM toTrash (gainOrTrash thief)
          return ()
        gainOrTrash thief c = do
          gain <- decide ("Do you want to gain a " ++ (show c) ++ " ?") thief
          if gain then discardCard c thief else trashCard c

          
-- Throne Room  Action  $4  Choose an Action card in your hand. Play it twice.
throneRoom :: Card
throneRoom = card "Throne Room" 4 noPoints throneRoomGamePlay
  where throneRoomGamePlay p = do
          currentHand <- hand p
          cards <- chooseCards "Choose action to play twice" p currentHand (upTo 1)
          unless (cards == []) $ playTwice (head cards) p
        playTwice c p = do
          putCardOnTable c
          cardGamePlay c p
          cardGamePlay c p
          
 