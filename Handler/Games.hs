{-# LANGUAGE DeriveGeneric #-}
module Handler.Games where

import Import
import Control.Concurrent
import Data.Map as Map
import Model.Game
import Model.Player
import GHC.Generics
import Data.Aeson as Aeson
import Data.Text (pack)
import Handler.Players
import Handler.ErrorCode

data PostGames = PostGames { numberOfPlayers :: Int } deriving (Generic,Show)
instance Aeson.FromJSON PostGames

data PostGamesId = PostGamesId { playerId :: Int } deriving (Generic,Show)
instance Aeson.FromJSON PostGamesId

getGameR :: Int -> Handler RepJson
getGameR gameId = do
  yesod <- getYesod
  allGames <- liftIO $ readMVar $ games yesod
  g <- liftIO $ readMVar (allGames Map.! gameId)
  renderGame (gameId,g)

postGameR :: Int -> Handler RepJson
postGameR gameId = do
  postGamesId <- parseJsonBody_
  player <- getPlayer (playerId postGamesId)
  game <- getGame gameId
  liftErrorCode illegalMove (join player g)
  yesod <- getYesod
  allGames <- liftIO $ takeMVar $ games yesod
  withGame gameId (fmap (fmap (\g -> (gameId,g))) (joinIfPossible player))

getGame :: Int -> Handler Game
getGame gameId = do
  yesod <- getYesod
  allGames <- liftIO $ readMVar $ games yesod
  case Map.lookup gameId allGames of
    Nothing -> returnError unknownGame
    Just mvg -> liftIO $ readMVar mvg f

putGame :: Int -> Game -> Handler ()
putGame gameId g = liftIO $ putMVar
  yesod <- getYesod
  allGames <- liftIO $ readMVar $ games yesod
  case Map.lookup gameId allGames of
    Nothing -> returnError unknownGame
    Just mvg -> liftIO $ putMVar mvg g

withGame :: (ToJSON a) => Int -> (Game -> Either ErrorCode (Game, a)) -> Handler RepJson
withGame gameId f = do
  yesod <- getYesod
  allGames <- liftIO $ readMVar $ games yesod
  res <- case Map.lookup gameId allGames of
    Nothing -> return $ Left unknownGame
    Just mvg -> liftIO $ updateMVar mvg f
  renderResponse res

updateMVar :: MVar a -> (a -> Either ErrorCode (a,b)) -> IO (Either ErrorCode b)
updateMVar mvar f = modifyMVar mvar (return . f') where
    f' a = case f a of
        Left err -> (a, Left err)
        Right (a',b) -> (a', Right b)
        
renderResponse :: (ToJSON a) => Either ErrorCode a -> Handler RepJson
renderResponse (Left err) = returnError err
renderResponse (Right a) = jsonToRepJson a

	
joinIfPossible :: Maybe Player -> Game -> Either ErrorCode Game
joinIfPossible Nothing _ = Left unknownPlayer
joinIfPossible (Just player) g = liftErrorCode illegalMove (join player g)

postGamesR :: Handler RepJson
postGamesR = do
  postGame <- parseJsonBody_
  yesod <- getYesod
  allGames <- liftIO $ takeMVar $ games yesod
  let g = newGame (numberOfPlayers postGame) 1
  mvg <- liftIO $ newMVar g
  let allGames' = Map.insert (Map.size allGames) mvg allGames 
  liftIO $ putMVar (games yesod) allGames'
  renderGame (Map.size allGames, g)

getGamesR :: Handler RepJson
getGamesR = do
  yesod <- getYesod
  render <- getUrlRender
  allGames <- liftIO $ readMVar $ games yesod
  jsonToRepJson $ Import.map (gameToJSON render) (Map.toList allGames)
   

initialGames :: Map.Map Int Player -> IO (Map.Map Int (MVar Game))
initialGames players = do
    g0 <- newMVar (newGame 2 0)
    g1 <- newMVar (rightGame $ join p0 (newGame 2 1))
    g2 <- newMVar (rightGame $ join p1 (newGame 2 2) >>= join p2)
    return $ Map.fromList [(0,g0),(1,g1),(2,g2)]
    where
	p0 = players Map.! 0
	p1 = players Map.! 1
	p2 = players Map.! 2
	rightGame (Right g) = g
	
---- json rep

renderGame :: (Int,Game) -> Handler RepJson
renderGame g = do
	render <- getUrlRender
	jsonToRepJson $ gameToJSON render g
	
gameToJSON render (gid,g) = Aeson.object [
	"status" .= show g, 
	"gid" .= gid, 
	"url" .= render (GameR gid)]
   
   
   
--
-- POST /games => { "numberOfPlayers": <Int> } -> { "url": <GamesUrl>, "seats" : [Maybe<Player>] }
-- GET /games/#Int 
-- POST /games/#Int => { "player-name": <Text> } -> { "url": <PlayerUrl>, "id" : String }
--    