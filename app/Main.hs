module Main where

import Client
import Server
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["client"] -> do
      putStrLn "Please, choose the size of the field and the goal"
      size <- readLn :: IO Int
      goal <- readLn :: IO Int
      startClient size goal
    ["server"] -> do
      putStrLn "Started application"
      startServer
