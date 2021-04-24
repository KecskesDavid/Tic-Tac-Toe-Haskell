module Game (
    Cell (..),
    Game (..),
    makeGame,
    rewriteGame,
    makeCell,
    makeCellX,
    makeCellO,
    isFreeCell,
    markX,
    markO,
    makeRowX,
    makeRowO
)where

import Data.List

data Cell = Cell {
    value :: Int
}deriving (Show) 

data Game = Game {
    table :: [[Cell]],
    playerMove :: Int
}deriving (Show) 


makeGame :: Game
makeGame = Game [[makeCell, makeCell, makeCell], [makeCell, makeCell, makeCell], [makeCell, makeCell, makeCell]] 1

-- Recreats a game with a new, marked cell
rewriteGame :: [[Cell]] -> Int -> Game
rewriteGame table playerMove = Game table playerMove

makeCell :: Cell
makeCell = Cell (-1)

makeCellX :: Cell
makeCellX = Cell 1

makeCellO :: Cell
makeCellO = Cell 0


-- Checks for free cells
isFreeCell :: Game -> Int -> Int -> Bool
isFreeCell game i j
    | i < 0 || j < 0 || i > 2 || j > 2 = False
    | value ((table game) !! i !! j) == -1 = True
    | otherwise = False

-- Marks a X on the table then returns it
-- curj should be 0
markX :: (Num a1, Num a, Eq a1, Eq a) =>[[Cell]] -> a1 -> a1 -> a -> [[Cell]]
markX table curi i j
    | curi == 3 = []
    | otherwise = makeRowX (head table) curi 0 i j : markX (tail table) (curi+1) i j

makeRowX :: (Num a, Eq a1, Eq a) => [Cell] -> a1 -> a -> a1 -> a -> [Cell]
makeRowX row curi curj i j
    | curj == 3 = []
    | curj == j && curi == i = makeCellX : makeRowX (tail row) curi (curj+1) i j
    | otherwise = (head row) : makeRowX (tail row) curi (curj+1) i j

-- Marks a 0 on the table then returns it
-- curi should be 0
markO :: (Num a1, Num a, Eq a1, Eq a) =>[[Cell]] -> a1 -> a1 -> a -> [[Cell]]
markO table curi i j
    | curi == 3 = []
    | otherwise = makeRowO (head table) curi 0 i j : markO (tail table) (curi+1) i j

makeRowO :: (Num a, Eq a1, Eq a) => [Cell] -> a1 -> a -> a1 -> a -> [Cell]
makeRowO row curi curj i j
    | curj == 3 = []
    | curj == j && curi == i = makeCellO : makeRowO (tail row) curi (curj+1) i j
    | otherwise = (head row) : makeRowO (tail row) curi (curj+1) i j