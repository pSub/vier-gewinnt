{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

import qualified Network.WebSockets as WS
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static
import Data.FileEmbed (embedDir)

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
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

-- A new game is spawn as Waiting, when
-- the second player connects the state
-- changes to Playing.
data GameState = Waiting Client
               | Playing Client Client

-- The list of all current games.
type ServerState = [GameState]

type GameId = Int

newServerState :: ServerState
newServerState = []

newLobby :: Lobby
newLobby = []

numGames :: ServerState -> Int
numGames = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any (checkClient (fst client))
             where checkClient :: Text -> GameState  -> Bool
                   checkClient c (Waiting (p, _)) = c == p
                   checkClient c (Playing (p1, _) (p2, _)) = c == p1 || c == p2

updateServerState :: Client -> GameId -> ServerState -> ServerState
updateServerState client id (g:ss)
                  | id == 0 = (updateGameState client g) : ss
                  | otherwise = g : (updateServerState client (pred id) ss)

updateGameState :: Client -> GameState -> GameState
updateGameState c (Waiting p) = Playing p c
updateGameState c (Playing p q)
                | fst c == fst p = Waiting q
                | fst c == fst q = Waiting p
updateGameState _ gs = gs

showGameState :: GameState -> Text
showGameState (Waiting (name, _)) = name
showGameState _ = ""

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
                  liftIO $ WS.sendSink sink $ WS.textData $ "GAMES: "
                         `mappend` (T.intercalate "," $ filter (/= "") $ map showGameState serverState)

                  -- Receive Name and GameId
                  msg <- WS.receiveData
                  let (name, gameId) = read $ T.unpack msg :: (Text, GameId)
                  case name of
                   _
                       | any ($ name)
                         [T.null, T.any isPunctuation, T.any isSpace] ->
                             WS.sendTextData ("ERROR: Name cannot " `mappend`
                             "contain punctuation or whitespace, and " `mappend`
                             "cannot be empty" :: Text)
                       | clientExists (name, sink) serverState ->
                         WS.sendTextData ("ERROR: User already exists" :: Text)
                       | otherwise -> do
                           liftIO $ modifyMVar_ state $ \s -> do
                                  WS.sendSink sink $ WS.textData ("OK" :: Text)
                                  if gameId == -1
                                     then return $ mappend [(Waiting (name, sink))] s
                                     else return $ updateServerState (name, sink) gameId s
                           lobby' <- liftIO $ readMVar lobby
                           state' <- liftIO $ readMVar state
                           forM_ lobby' (\x -> liftIO $ WS.sendSink x $ WS.textData $ "GAMES: " `mappend` (T.intercalate "," $ map showGameState state'))

                           if gameId == -1 
                              then do
                                   liftIO $ WS.sendSink sink $ WS.textData ("FIRST" :: Text)
                                   talk state (numGames state' - 1) (name, sink)
                              else do
                                   liftIO $ WS.sendSink sink $ WS.textData ("SECOND" :: Text)
                                   liftIO $ broadcast "STARTED" (state' !! gameId)
                                   talk state gameId (name, sink)

talk :: WS.Protocol p => MVar ServerState -> GameId -> Client -> WS.WebSockets p ()
talk state gameId client@(user, _) = flip WS.catchWsError catchDisconnect $ do
    msg <- WS.receiveData
    liftIO $ T.putStrLn msg
    liftIO $ readMVar state >>= broadcast msg . (!! gameId)
    talk state gameId client
  where
    catchDisconnect e = case fromException e of
        Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ \s -> do
            let s' = updateServerState client gameId s
            broadcast (user `mappend` " disconnected") (s' !! gameId)
            return s'
        _ -> return ()
