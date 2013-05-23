{-# LANGUAGE DeriveGeneric #-}
module Handler.Games where

import Import
import Control.Concurrent
import Data.Map as Map
import Model.Game (newGame, join)
import GHC.Generics
import Data.Aeson as Aeson
import Data.Monoid
import Data.Text (pack)

data PostGame = PostGame { numberOfPlayers :: Int } deriving (Generic,Show)
instance Aeson.FromJSON PostGame

data PostGameId = PostGameId { name :: Text } deriving (Generic,Show)
instance Aeson.FromJSON PostGameId

getGamesIdR :: Int -> Handler RepJson
getGamesIdR gameId = do
  yesod <- getYesod
  allGames <- liftIO $ readMVar $ games yesod
  let g = allGames Map.! gameId
  jsonToRepJson g

postGamesIdR :: Int -> Handler RepJson
postGamesIdR gameId = do
  postGameId <- parseJsonBody_
  yesod <- getYesod
  allGames <- liftIO $ takeMVar $ games yesod
  case Map.lookup gameId allGames of
  	Nothing -> jsonToRepJson $ Aeson.object ["error" .= pack "Game does not exist"]
  	Just g -> case join (name postGameId) g of
  		Nothing -> jsonToRepJson $ Aeson.object ["error" .= pack "Game is already full"]
  		Just g' -> do
  			liftIO $ putMVar (games yesod) (Map.insert gameId g' allGames)
  			jsonToRepJson g'

postGamesR :: Handler RepJson
postGamesR = do
  postGame <- parseJsonBody_
  yesod <- getYesod
  allGames <- liftIO $ takeMVar $ games yesod
  let allGames' = Map.insert (Map.size allGames) (newGame (numberOfPlayers postGame)) allGames 
  liftIO $ putMVar (games yesod) allGames'
  jsonToRepJson $ Aeson.object ["current-size" .= Map.size allGames' ]

getGamesR :: Handler RepJson
getGamesR = do
  yesod <- getYesod
  allGames <- liftIO $ readMVar $ games yesod
  jsonToRepJson $ Aeson.object (Import.map (\(id,g) -> pack (show id) .= g) (Map.toList allGames)) 
   