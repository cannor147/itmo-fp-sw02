{-# LANGUAGE DeriveGeneric #-}

module GameState
  ( createGame
  , makeMove
  , findWinner
  , findFreePositions
  , Position
  , Squad (..)
  , Cell (..)
  , State (..)
  , Board (..)
  , GameState (..)
  ) where

import GHC.Generics ( Generic )
import Data.Aeson
import Data.Foldable
import Control.Applicative (liftA2)
import Data.Maybe (maybeToList, isJust)

type Position = (Int, Int)
data Squad    = X | O deriving (Generic, Show, Eq)
data Cell     = Empty | NonEmpty Squad deriving (Generic, Show, Eq)
data State    = Playing | End (Maybe Squad) deriving (Generic, Show, Eq)

data Board = Board
  { boardSize  :: Int
  , boardGoal  :: Int
  , boardCells :: [[Cell]]
  } deriving (Generic)

data GameState = GameState
  { gameBoard  :: Board
  , gameSquad  :: Squad
  , gameState  :: State
  } deriving (Generic)

changePlayer :: Squad -> Squad
changePlayer O = X
changePlayer X = O

createGame :: Int -> Int -> GameState
createGame size goal = GameState
  { gameBoard  = Board size goal $ replicate size (replicate size Empty)
  , gameSquad  = X
  , gameState  = Playing
  }

makeMove :: GameState -> Position -> GameState
makeMove game (x, y) = handleGameOver $ game
  { gameBoard = if check then board { boardCells = newCells } else board
  , gameSquad = if check then changePlayer oldPlayer else oldPlayer
  } where
    oldPlayer          = gameSquad game
    board              = gameBoard game
    size               = boardSize board
    check              = min x y >= 0 && max x y < size && getCell board (x, y) == Empty
    newCells           = prefix <> [prefix' <> [NonEmpty oldPlayer] <> tail suffix'] <> tail suffix
    (prefix, suffix)   = splitAt x $ boardCells $ gameBoard game
    (prefix', suffix') = splitAt y $ head suffix

handleGameOver :: GameState -> GameState
handleGameOver game = game { gameState = kek } where
  board  = gameBoard game
  isDraw = all (notElem Empty) $ boardCells board
  winner = findWinner board
  kek
    | isJust winner = End winner
    | isDraw        = End Nothing
    | otherwise     = Playing

getCell :: Board -> Position -> Cell
getCell board (x, y) = boardCells board !! x !! y

findWinner :: Board -> Maybe Squad
findWinner board = fmap head $ find ((==) goal . length) $ map (foldr check []) $ content initAll where
  size      = boardSize board
  goal      = boardGoal board
  indeces   = [0 .. size - 1]
  diagonals = [if k > 0 then take k indeces else drop k indeces | k <- [-size + 1 .. size - 1]]
  initAll   = initV ++ initH ++ initMD ++ initAD
  initV     = [zip indeces (repeat x) | x <- indeces]
  initH     = [zip (repeat x) indeces | x <- indeces]
  initMD    = liftA2 zip (reverse diagonals) diagonals
  initAD    = liftA2 zip diagonals (fmap reverse diagonals)
  content   = map (map $ cellToMaybe . getCell board)
  check a b
    | length b == goal                     = b
    | not (null b) && (a == Just (last b)) = b ++ maybeToList a
    | otherwise                            = maybeToList a

findFreePositions :: GameState -> [Position]
findFreePositions game = filter ((==) Empty . getCell board) generateIndeces where
  board             = gameBoard game
  size              = boardSize board
  generateIndeces   = map (\k -> (k `div` size, k `rem` size)) [0 .. size * size - 1]

cellToMaybe :: Cell -> Maybe Squad
cellToMaybe Empty        = Nothing
cellToMaybe (NonEmpty v) = Just v

instance ToJSON Squad     where toEncoding = genericToEncoding defaultOptions
instance ToJSON Cell      where toEncoding = genericToEncoding defaultOptions
instance ToJSON Board     where toEncoding = genericToEncoding defaultOptions
instance ToJSON State     where toEncoding = genericToEncoding defaultOptions
instance ToJSON GameState where toEncoding = genericToEncoding defaultOptions

instance FromJSON Squad
instance FromJSON Cell
instance FromJSON Board
instance FromJSON State
instance FromJSON GameState
