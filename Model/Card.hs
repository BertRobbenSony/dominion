module Model.Card(Board, Card, initialDeck, initialBoard) where

import Model.Game
import Prelude

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

moneyCard :: String -> Int -> Card
moneyCard n v = card n v (\_ -> 0) noAction

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
curse = card "Curse" 0 (\_ -> -1) noAction

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

cellar :: Card
cellar = chooseCardsAction "Cellar" 2 "Choose cards to discard" cellarSteps
  where cellarSteps p = cardsStep (discardCard p) <.> cardsStep (\_ -> drawCard p)
  
-- Cellar	Action	$2	+1 Action
-- Discard any number of cards.
-- +1 Card per card discarded.
 
chapel :: Card
chapel = chooseCardsAction "Chapel" 2 "Choose cards to trash" chapelSteps
  where chapelSteps p cards = if length cards > 4 
          then errorStep "Maximum 4 cards please"
          else cardsStep (trashCard p) cards
-- Chapel	Action	$2	Trash up to 4 cards from your hand.

moat :: Card
moat = action "Moat" 2 (drawCard <.> drawCard)
-- Moat	Action � Reaction	$2	+2 Cards
-- When another player plays an Attack card, you may reveal this from your hand. If you do, you are unaffected by that Attack.

chancellor :: Card
chancellor = choiceAction "Chancellor" 3 "Put deck into discard pile?" chancellorSteps
  where chancellorSteps p b = changeMoney p 2 <.> (if b then putDeckOnDiscardPile p else idStep)
-- Chancellor	Action	$3	+$2
-- You may immediately put your deck into your discard pile.
-- playChancellor = changeMoney p 2 <.> decisionStep (\b -> if b then discardDeck p else id)
 
village :: Card
village = action "Village" 3 ((changeActions 2) <.> drawCard)
-- Village	Action	$3	+1 Card; +2 Actions.

woodcutter :: Card
woodcutter = action "Woodcutter" 3 ((changeBuy 1) <.> (changeMoney 2))
-- Woodcutter	Action	$3	+1 Buy; +$2.

workshop :: Card
workshop = chooseCardsAction "Workshop" 3 "Choose a card costing up to $4" workshopSteps
  where workshopSteps p ([c]) = if cardValue c > 4 then errorStep "Max $4" else takeCardFromBoard p c
        workshopSteps p _ = errorStep "Choose one card please"  
-- Workshop	Action	$3	Gain a card costing up to $4.

bureaucrat :: Card
bureaucrat = undefined
-- Bureaucrat	Action � Attack	$4	Gain a silver card; put it on top of your deck. Each other player reveals a Victory card from his hand and puts it on his deck (or reveals a hand with no Victory cards).

feast :: Card
feast = chooseCardsAction "Feast" 4 "Choose a card from board costing up to $5" feastSteps
  where feastSteps p ([c]) = if cardValue c > 5 then errorStep "Max $5" else takeCardFromBoard p c <.> takeCardFromTable p feast 
        feastSteps p _ = errorStep "Choose one card please"
-- Feast	Action	$4	Trash this card. Gain a card costing up to $5.

gardens :: Card
gardens = undefined
--  Gardens	Victory	$4	Worth 1 Victory for every 10 cards in your deck (rounded down).

militia :: Card
militia = action "Militia" 4 (\p -> changeMoney p 2 <.> eachOtherPlayer p discardDownToThree)
  where discardDownToThree p = handStep p (discardCards p)
        discardCards p cards = if length cards <= 3 
          then idStep 
          else cardStep "Choose cards to discard (down to 3 in hand)" p (doDiscardCards p)
        doDiscardCards p cardsToDiscard = cardsStep (discardCard p) cardsToDiscard <.> (discardDownToThree p) 
-- Militia	Action � Attack	$4	+$2
-- Each other player discards down to 3 cards in his hand.

moneylender :: Card
moneylender = choiceAction 4 "Trash a copper for $3?" trashCopper
  where trashCopper p trash = if trash then changeMoney p 3 else idStep   
-- Moneylender	Action	$4	Trash a Copper from your hand. If you do, +$3.

remodel :: Card
remodel = undefined
-- Remodel	Action	$4	Trash a card from your hand. Gain a card costing up to $2 more than the trashed card.

smithy :: Card
smithy = action "Smithy" 4 (drawCard <.> drawCard <.> drawCard)
-- Smithy	Action	$4	+3 Cards.

spy :: Card
spy = undefined
-- Spy	Action � Attack	$4	+1 Card; +1 Action
-- Each player (including you) reveals the top card of his deck and either discards it or puts it back, your choice.

thief :: Card
thief = undefined
-- Thief	Action � Attack	$4	Each other player reveals the top 2 cards of his deck. If they revealed any Treasure cards, they trash one of them that you choose. You may gain any or all of these trashed cards. They discard the other revealed cards.

throneRoom :: Card
throneRoom = chooseCardsAction "Throne Room" 4 playThroneRoom
  playThroneRom p ([c]) = (cardActionPlay c p) <.> (cardActionPlay c p)
-- Throne Room	Action	$4	Choose an Action card in your hand. Play it twice.

councilRoom :: Card
councilRoom = action "Council Room" 5 playCouncilRoom
  where playCouncilRoom p = (updateBuy p 1) <.> (drawCard p) <.> (drawCard p) <.> (drawCard p) <.> (drawCard p) <.> eachOtherPlayer p drawCard
-- Council Room	Action	$5	+4 Cards; +1 Buy
-- Each other player draws a card.

festival :: Card
festival = action "Festival" 5 playFestival
  where playFestival p = updateBuy p 1 <.> updateMoney p 2 <.> updateAction p 2
-- Festival	Action	$5	+2 Actions, +1 Buy; +$2.

laboratory :: Card
laboratory = action "Laboratory" 5 playLab
  where playLab p = drawCard p <.> drawCard p <.> updateAction p 1
-- Laboratory	Action	$5	+2 Cards; +1 Action.

library :: Card
library = undefined
-- Library	Action	$5	Draw until you have 7 cards in hand. You may set aside any Action cards drawn this way, as you draw them; discard the set aside cards after you finish drawing.

market :: Card
market = action "Market" 5 playMarket
  where playMarket p = drawCard p <.> updateAction p 1 <.> updateBuy p 1 <.> updateMoney p 1
-- Market	Action	$5	+1 Card; +1 Action; +1 Buy; +$1.

mine :: Card
mine = undefined
-- Mine	Action	$5	Trash a Treasure card from your hand. Gain a Treasure card costing up to $3 more; put it into your hand.

witch :: Card
witch = action "Witch" 5 playWitch
  where playWitch p = drawCard p <.> drawCard p <.> eachOtherPlayer p takeCurse
        takeCurse p b ps = case takeCardFromBoard p curse b ps of
          Right t => Right t
          _ => Right ([], b, ps) 
-- Witch	Action � Attack	$5	+2 Cards
-- Each other player gains a Curse card.

adventurer :: Card
adventurer = undefined
-- Adventurer	Action	$6	Reveal cards from your deck until you reveal 2 Treasure cards. Put those Treasure cards in your hand and discard the other revealed cards.
