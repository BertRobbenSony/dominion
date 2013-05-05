module Model.Card(Board, Card, initialDeck, initialBoard) where

import Prelude

data CardType = Treasure | Victory | Action | Curse

data Card = Card { name :: String, value :: Int, victoryPoints :: [Card] -> Int, cardType :: CardType }

instance Show Card where                                                                                       
    show c = name c                                                                            
    
dominionCards :: [Card]
dominionCards = [cellar, chapel, moat, chancellor, village, woodcutter, workshop, bureaucrat,
	feast, gardens, militia, moneylender, remodel, smithy, spy, thief, throneRoom, 
	councilRoom, festival, laboratory, library, market, mine, witch, adventurer]

initialDeck :: [Card] 
initialDeck = [ copper, copper, copper, copper, copper, copper, copper, estate, estate, estate ]

type Board = [(Card, Int)]

initialBoard :: Int -> Board
initialBoard n = let victories = if n == 2 then 8 else 12
	in zip (take 10 dominionCards) (repeat 10) ++
       [ (copper, 50), (silver, 50), (gold, 50), 
         (estate, victories), (duchy, victories), (province, victories),
		 (curse, (n - 1) * 10) ]  
	
money :: String -> Int -> Card
money n v = Card n v (\_ -> 0) Treasure

copper :: Card
copper = money "Copper" 0
silver :: Card
silver = money "Silver" 2
gold :: Card
gold = money "Gold" 6

victory :: String -> Int -> Int -> Card
victory n c vp = Card n c (\_ -> vp) Victory

estate :: Card
estate = victory "Estate" 2 1
duchy :: Card
duchy = victory "Duchy" 5 3
province :: Card
province = victory "Province" 8 6

curse :: Card
curse = Card "Curse" 0 (\_ -> -1) Curse

--
-- actions
--

action :: String -> Int -> Card
action n c = Card n c (\_ -> 0) Action

cellar :: Card
cellar = action "Cellar" 2
-- Cellar	Action	$2	+1 Action
-- Discard any number of cards.
-- +1 Card per card discarded.

chapel :: Card
chapel = action "Chapel" 2
-- Chapel	Action	$2	Trash up to 4 cards from your hand.

moat :: Card
moat = action "Moat" 2
-- Moat	Action – Reaction	$2	+2 Cards
-- When another player plays an Attack card, you may reveal this from your hand. If you do, you are unaffected by that Attack.

chancellor :: Card
chancellor = action "Chancellor" 3
-- Chancellor	Action	$3	+$2
-- You may immediately put your deck into your discard pile.

village :: Card
village = action "Village" 3
-- Village	Action	$3	+1 Card; +2 Actions.

woodcutter :: Card
woodcutter = action "Woodcutter" 3
-- Woodcutter	Action	$3	+1 Buy; +$2.

workshop :: Card
workshop = action "Workshop" 3
-- Workshop	Action	$3	Gain a card costing up to $4.

bureaucrat :: Card
bureaucrat = action "Bureaucrat" 4
-- Bureaucrat	Action – Attack	$4	Gain a silver card; put it on top of your deck. Each other player reveals a Victory card from his hand and puts it on his deck (or reveals a hand with no Victory cards).

feast :: Card
feast = action "Feast" 4
-- Feast	Action	$4	Trash this card. Gain a card costing up to $5.

gardens :: Card
gardens = Card "Gardens" 4 (\cards -> length cards `div` 10) Victory
--  Gardens	Victory	$4	Worth 1 Victory for every 10 cards in your deck (rounded down).

militia :: Card
militia = action "Militia" 4
-- Militia	Action – Attack	$4	+$2
-- Each other player discards down to 3 cards in his hand.

moneylender :: Card
moneylender = action "Moneylender" 4
-- Moneylender	Action	$4	Trash a Copper from your hand. If you do, +$3.

remodel :: Card
remodel = action "Remodel" 4
-- Remodel	Action	$4	Trash a card from your hand. Gain a card costing up to $2 more than the trashed card.

smithy :: Card
smithy = action "Smithy" 4
-- Smithy	Action	$4	+3 Cards.

spy :: Card
spy = action "Spy" 4
-- Spy	Action – Attack	$4	+1 Card; +1 Action
-- Each player (including you) reveals the top card of his deck and either discards it or puts it back, your choice.

thief :: Card
thief = action "Thief" 4
-- Thief	Action – Attack	$4	Each other player reveals the top 2 cards of his deck. If they revealed any Treasure cards, they trash one of them that you choose. You may gain any or all of these trashed cards. They discard the other revealed cards.

throneRoom :: Card
throneRoom = action "Throne room" 4
-- Throne Room	Action	$4	Choose an Action card in your hand. Play it twice.

councilRoom :: Card
councilRoom = action "Council Room" 5
-- Council Room	Action	$5	+4 Cards; +1 Buy
-- Each other player draws a card.

festival :: Card
festival = action "Festival" 5
-- Festival	Action	$5	+2 Actions, +1 Buy; +$2.

laboratory :: Card
laboratory = action "Laboratory" 5
-- Laboratory	Action	$5	+2 Cards; +1 Action.

library :: Card
library = action "Library" 5
-- Library	Action	$5	Draw until you have 7 cards in hand. You may set aside any Action cards drawn this way, as you draw them; discard the set aside cards after you finish drawing.

market :: Card
market = action "Market" 5
-- Market	Action	$5	+1 Card; +1 Action; +1 Buy; +$1.

mine :: Card
mine = action "Mine" 5
-- Mine	Action	$5	Trash a Treasure card from your hand. Gain a Treasure card costing up to $3 more; put it into your hand.

witch :: Card
witch = action "Witch" 5
-- Witch	Action – Attack	$5	+2 Cards
-- Each other player gains a Curse card.

adventurer :: Card
adventurer = action "Adventurer" 6
-- Adventurer	Action	$6	Reveal cards from your deck until you reveal 2 Treasure cards. Put those Treasure cards in your hand and discard the other revealed cards.  