{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

import qualified Network.WebSockets as WS
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static
import Data.FileEmbed (embedDir)

import Data.Maybe (fromJust)
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_, readMVar)
import Control.Exception (fromException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)

-- The clients are identified by names
-- and the sinks are used to send messages
-- to them.
type Client = (Text, WS.Sink WS.Hybi00)

-- The lobby is a place where players live
-- until they join/create a game. This is used
-- to send updates of the list of games to them.
type Lobby = [WS.Sink WS.Hybi00]

-- The game identifier.
type GameId = Text

-- A new game is spawn as Waiting, when
-- the second player connects the state
-- changes to Playing.
data GameState = Waiting Client
               | Playing Client Client

-- The list of all current games.
type ServerState = Map GameId GameState

newServerState :: ServerState
newServerState = Map.empty

newLobby :: Lobby
newLobby = []

numGames :: ServerState -> Int
numGames = Map.size

updateServerState :: Client -> GameId -> ServerState -> ServerState
updateServerState client key state = Map.update (updateGameState client) key state

updateGameState :: Client -> GameState -> Maybe GameState
updateGameState c (Waiting p) = Just $ Playing p c
updateGameState _ (Playing _ _) = Nothing

showWaitingGames :: ServerState -> Text
showWaitingGames = (mappend "GAMES: ") . T.intercalate "," . filter (/= "") . Map.keys

broadcast :: Text -> GameState -> IO ()
broadcast message gameState = do
                  T.putStrLn message
                  case gameState of
                       (Waiting (_, sink)) -> WS.sendSink sink $ WS.textData message
                       (Playing (_, sink1) (_, sink2)) -> do
                                WS.sendSink sink1 $ WS.textData message
                                WS.sendSink sink2 $ WS.textData message

main :: IO ()
main = do
     putStrLn "http://localhost:3000/client.html"
     state <- newMVar newServerState
     lobby <- newMVar newLobby
     Warp.runSettings Warp.defaultSettings
       { Warp.settingsPort = 3000
       , Warp.settingsIntercept = WaiWS.intercept (application lobby state)
       } staticApp

staticApp :: Network.Wai.Application
staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "static")

application :: MVar Lobby ->  MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi00 ()
application lobby state rq = do
            WS.acceptRequest rq
            msg <- WS.receiveData
            liftIO $ T.putStrLn msg
            if msg /= "HI"
               then WS.sendTextData ("Wrong announcement" :: Text)
               else do
                  sink <- WS.getSink
                  liftIO $ modifyMVar_ lobby $ \l ->
                         return (sink : l)
                  serverState <- liftIO $ readMVar state
                  liftIO $ WS.sendSink sink $ WS.textData $ showWaitingGames serverState

                  -- Receive Name and GameId
                  msg <- WS.receiveData
                  let (user, game, newgame) = read $ T.unpack msg :: (Text, GameId, GameId)
                  case user of
                   _
                       | any ($ user)
                         [T.null, T.any isPunctuation, T.any isSpace] ->
                             WS.sendTextData ("ERROR: Username cannot " `mappend`
                             "contain punctuation or whitespace, and " `mappend`
                             "cannot be empty" :: Text)
                       | game == "-1" && any ($ newgame)
                         [T.null, T.any isPunctuation, T.any isSpace] ->
                             WS.sendTextData ("ERROR: Username cannot " `mappend`
                             "contain punctuation or whitespace, and " `mappend`
                             "cannot be empty" :: Text)
                       | game == "-1" && Map.member newgame serverState ->
                         WS.sendTextData ("ERROR: Game already exists" :: Text)
                       | otherwise -> do
                           liftIO $ modifyMVar_ state $ \s -> do
                                  WS.sendSink sink $ WS.textData ("OK" :: Text)
                                  if game == "-1"
                                     then do
                                          liftIO $ WS.sendSink sink $ WS.textData ("FIRST" :: Text)
                                          return $ Map.insert newgame (Waiting (user, sink)) s
                                     else do
                                          liftIO $ WS.sendSink sink $ WS.textData ("SECOND" :: Text)
                                          liftIO $ broadcast "STARTED" $ fromJust $ Map.lookup game s
                                          return $ updateServerState (user, sink) game s
                           state' <- liftIO $ readMVar state
                           lobby <- liftIO $ readMVar lobby
                           forM_ lobby (\x -> liftIO $ WS.sendSink x $ WS.textData $ showWaitingGames state')
                           if game == "-1"
                              then do
                                   talk state newgame (user, sink)
                              else do
                                   talk state game (user, sink)

talk :: WS.Protocol p => MVar ServerState -> GameId -> Client -> WS.WebSockets p ()
talk state game client@(user, _) = flip WS.catchWsError catchDisconnect $ do
    msg <- WS.receiveData
    liftIO $ T.putStrLn msg
    liftIO $ readMVar state >>= broadcast msg . (fromJust . Map.lookup game)
    talk state game client
  where
    catchDisconnect e = case fromException e of
        Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ \s -> do
            let s' = updateServerState client game s
            broadcast (user `mappend` " disconnected") (fromJust $ Map.lookup game s')
            return s'
        _ -> return ()