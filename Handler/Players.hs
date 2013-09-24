{-# LANGUAGE DeriveGeneric #-}
module Handler.Players where

import Import
import Control.Concurrent
import Data.Map as Map
import Model.Player
import GHC.Generics
import Data.Aeson as Aeson
import Data.Text (pack)

data PostPlayers = PostPlayers { name :: Text } deriving (Generic,Show)
instance Aeson.FromJSON PostPlayers

postPlayersR :: Handler RepJson
postPlayersR = do
  postPlayer <- parseJsonBody_
  yesod <- getYesod
  allPlayers <- liftIO $ takeMVar $ players yesod
  let newPid = Map.size allPlayers
      newPlayer = Player (name postPlayer) newPid
      allPlayers' = Map.insert newPid newPlayer allPlayers 
  liftIO $ putMVar (players yesod) allPlayers'
  renderPlayer newPlayer

getPlayerR :: Int -> Handler RepJson
getPlayerR pId = do
    p <- getPlayer pId
    renderPlayer p

getPlayersR :: Handler RepJson
getPlayersR = do
  yesod <- getYesod
  render <- getUrlRender
  allPlayers <- liftIO $ readMVar $ players yesod
  jsonToRepJson $ Import.map (playerToJSON render)(Map.elems allPlayers)

getPlayer :: Int -> Handler Player
getPlayer pId = do
  yesod <- getYesod
  allPlayers <- liftIO $ readMVar $ players yesod
  case Map.lookup pId allPlayers of
    Nothing -> returnError unknownPlayer
    Just mvar -> liftIO $ readMVar mvar

renderPlayer :: Player -> Handler RepJson
renderPlayer p = do
	render <- getUrlRender
	jsonToRepJson $ playerToJSON render p
	
playerToJSON render (Player name pid) = Aeson.object [
	"playerName" .= name, 
	"pid" .= pid, 
	"url" .= render (PlayerR pid)]
  
initialPlayers :: Map.Map Int Player
initialPlayers = Map.fromList (playerList ["Bert", "Neo", "Elise", "Thomas"])
	where playerList names = Import.map (\(id,name) -> (id,Player (pack name) id)) (zip [0..] names)