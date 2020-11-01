module Client
  ( startClient
  ) where

import GameState
import Network.HTTP.Simple
import Data.Aeson.Types (ToJSON)
import Configuration
import UI

doGet :: (ToJSON a) => String -> a -> IO GameState
doGet path object = do
  fullPath <- parseRequest $ "GET" <> " " <> protocol <> "://" <> host <> ":" <> show port <> path
  let request = setRequestBodyJSON object fullPath
  response <- httpJSON request :: IO (Response GameState)
  return $ getResponseBody response

gameHandler :: GameState -> Position -> IO GameState
gameHandler game position = case gameState game of
  Playing -> doGet "/move" =<< pure (makeMove game position)
  End _ -> doGet "/create" (boardSize $ gameBoard game, boardGoal $ gameBoard game)

startClient :: Int -> Int -> IO ()
startClient size goal = do
  game <- doGet "/create" (size, goal)
  display game gameHandler
