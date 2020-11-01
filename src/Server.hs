{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Server
  ( startServer
  ) where

import Control.Monad.IO.Class
import GameState
import Network.Wai.Handler.Warp
import Servant
import System.Random
import Configuration

type MakeMoveRequest   = "move" :> ReqBody '[JSON] GameState :> Get '[JSON] GameState
type CreateGameRequest = "create" :> ReqBody '[JSON] (Int, Int) :> Get '[JSON] GameState

type API = MakeMoveRequest :<|> CreateGameRequest

server :: Server API
server = makeMoveRequest :<|> createGameRequest

api :: Proxy API
api = Proxy

createGameRequest :: (Int, Int) -> Handler GameState
createGameRequest (size, goal) = do
  liftIO $ putStrLn "Created new game"
  computerSquad <- liftIO (randomIO :: IO Bool) >>= \r -> if r then return X else return O
  if computerSquad == X then
    makeMoveRequest $ createGame size goal
  else 
    pure $ createGame size goal

makeMoveRequest :: GameState -> Handler GameState
makeMoveRequest game = do
  if gameState game == Playing then do
    let freePositions = findFreePositions game
    positionIndex <- liftIO (randomRIO (0, length freePositions - 1))
    let position = freePositions !! positionIndex
    liftIO $ putStrLn $ "Generated move " <> show position
    return $ makeMove game position
  else do
    liftIO $ putStrLn "No available moves"
    return game

application :: Application
application = serve api server

startServer :: IO ()
startServer = run port application
