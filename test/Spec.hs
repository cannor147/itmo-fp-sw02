module Main where

import GameState
import Test.Hspec


emptyCells :: [[Cell]]
emptyCells = [ [Empty, Empty, Empty]
             , [Empty, Empty, Empty]
             , [Empty, Empty, Empty]
             ]

xWinnerCells :: [[Cell]]
xWinnerCells = [ [NonEmpty X, NonEmpty O, Empty     ]
               , [Empty     , NonEmpty X, NonEmpty O]
               , [Empty     , Empty     , NonEmpty X]
               ]

oWinnerCells :: [[Cell]]
oWinnerCells = [ [NonEmpty X, NonEmpty X, Empty     ]
               , [NonEmpty O, NonEmpty X, NonEmpty X]
               , [NonEmpty O, NonEmpty O, NonEmpty O]
               ]

noWinnerCells :: [[Cell]]
noWinnerCells = [ [Empty     , NonEmpty X, NonEmpty X]
                , [NonEmpty O, NonEmpty X, NonEmpty X]
                , [Empty     , NonEmpty O, NonEmpty O]
                ]

fiveSizeCells :: [[Cell]]
fiveSizeCells = [ [NonEmpty X, NonEmpty X, NonEmpty X, NonEmpty X, Empty     ]
                , [Empty     , Empty     , Empty     , Empty     , Empty     ]
                , [Empty     , Empty     , Empty     , Empty     , Empty     ]
                , [Empty     , Empty     , Empty     , Empty     , Empty     ]
                , [NonEmpty O, NonEmpty O, NonEmpty O, Empty     , Empty     ]
                ]

prepareBoard :: [[Cell]] -> Int -> Board
prepareBoard cells goal = Board (length cells) goal cells

prepareGame :: [[Cell]] -> Int -> Squad -> GameState
prepareGame cells goal currentSquad = GameState (Board (length cells) goal cells) currentSquad Playing

testWinner :: SpecWith ()
testWinner = describe "test winner" $ do
  it "empty (draw)"                  $ shouldBe (findWinner $ prepareBoard emptyCells    3) Nothing
  it "x is winner"                   $ shouldBe (findWinner $ prepareBoard xWinnerCells  3) (Just X)
  it "o is winner"                   $ shouldBe (findWinner $ prepareBoard oWinnerCells  3) (Just O)
  it "draw"                          $ shouldBe (findWinner $ prepareBoard noWinnerCells 3) Nothing
  it "x is winner (5 size, 4 goal)"  $ shouldBe (findWinner $ prepareBoard fiveSizeCells 4) (Just X)

testMove :: SpecWith ()
testMove = describe "test move" $ do
  let game1 = makeMove (prepareGame emptyCells    3 X) (1, 1)
  let game2 = makeMove (prepareGame noWinnerCells 3 O) (2, 0)
  it "test move in empty"                  $ shouldBe (gameState game1) Playing
  it "test move in draw (now o is winner)" $ shouldBe (gameState game2) (End (Just O))

main :: IO ()
main = hspec $ do
  testWinner
  testMove