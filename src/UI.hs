{-# LANGUAGE BlockArguments #-}

module UI
  ( display
  ) where

import GameState
import Control.Monad.Cont (forM_, when)
import Data.List (intercalate)
import System.Console.ANSI
import Data.List.Split

display :: GameState -> (GameState -> Position -> IO GameState) -> IO ()
display game gameHandler = do
  let currentSquad = gameSquad game
  let size = boardSize $ gameBoard game
  setCursorPosition 0 0
  setTitle "Tic-tac-toe"
  clearScreen

  case gameState game of
    Playing -> do
      printBoard (gameBoard game)
      printText ("It's you turn, " <> show currentSquad <> "! Print position if format 'x, y' or type 'exit'.") NormalIntensity White
      input <- getLine
      when (input /= "exit") $ do
        let position = splitOn "," input
        x <- pure $ read (head position) :: IO Int
        y <- pure $ read (last position) :: IO Int
  
        if min x y < 0 || max x y >= size then do
          printText "Invalid indeces. Please, print any key and try again or type 'exit'." NormalIntensity White
          input' <- getLine
          when (input' /= "exit") $ do
            display game gameHandler
        else do
          newGame <- gameHandler game (x, y)
          display newGame gameHandler
    End f -> do
      printEnd f (gameBoard game)
      printText "Print any key to continue or type 'exit'." NormalIntensity White
      input <- getLine
      when (input /= "exit") $ do
        let x = -1
        let y = -1
        newGame <- gameHandler game (x, y)
        display newGame gameHandler

printEnd :: Maybe Squad -> Board -> IO ()
printEnd winner board = do
  printBoard board
  printWinner winner

printBoard :: Board -> IO ()
printBoard board = do
  forM_ (boardCells board) $ \line -> do
    printText (intercalate " | " (map cellToString line)) NormalIntensity White

printWinner :: Maybe Squad -> IO ()
printWinner Nothing = printText "It's a draw!" BoldIntensity Yellow
printWinner (Just X) = printText "X wins!" BoldIntensity Blue
printWinner (Just O) = printText "O wins!" BoldIntensity Red

cellToString :: Cell -> String
cellToString Empty = " "
cellToString (NonEmpty X) = "X"
cellToString (NonEmpty O) = "O"

printText ::String -> ConsoleIntensity -> Color -> IO ()
printText text intensity color = do
  setSGR [ SetConsoleIntensity intensity
         , SetColor Foreground Vivid color
         ]
  putStrLn text
