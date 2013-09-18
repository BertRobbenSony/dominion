{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Handler.Moves where

import Import
import Control.Concurrent
import Data.Map as Map
import Model.Player
import Model.DominionGamePlay
import GHC.Generics
import Data.Aeson as Aeson
import Data.Text (Text,pack,unpack)
import Model.Game
import Model.GameState (board)
import Control.Monad
import Handler.ErrorCode
import Handler.Games

data PostMoves = PostMoves { choice :: Maybe [Text], decision :: Maybe Bool  } deriving (Generic,Show)
instance Aeson.FromJSON PostMoves

postMovesR :: Int -> Handler RepJson
postMovesR gameId = do
  postMoves <- parseJsonBody_
  withGame gameId (liftDominionGame (liftErrorCode illegalMove (performMove postMoves)))

performMove :: PostMoves -> DominionGame -> Either Text DominionGame
performMove (PostMoves (Just cs) Nothing) dg = do
    cards <- asCards dg cs
    playerChoice cards dg
performMove (PostMoves Nothing (Just b)) dg = playerDecision b dg
performMove _ _ = Left malformedBody

asCards :: DominionGame -> [Text] -> Either Text [Card]
asCards dg cardNames = forM cardNames cardWithName
    where cardWithName = lookupCard dg

lookupCard :: DominionGame -> Text -> Either Text Card
lookupCard g name = findCard $ Import.map fst (board (dominionGameState g))
    where findCard [] = Left "Unknown card"
          findCard (c:cs) = if cardName c == name then Right c else findCard cs