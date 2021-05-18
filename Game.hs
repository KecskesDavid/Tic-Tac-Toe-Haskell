module Game (
    Cell (..),
    Game (..),
    makeGame,
    rewriteGame,
    makeCell,
    makeCellX,
    makeCellO,
    isFreeCell,
    isFreeCellT,
    markX,
    markO,
    makeRowX,
    makeRowO,
    returnWinner,
    checkDiagonals,
    checkColumns,
    checkRows,
    checkRow,
    printCurrentStateOfGame,
    makeHeaderStr,
    makeTableStr,
    makeRowStr
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

-- isFreeCellT :: Game -> Int -> Int -> Bool
isFreeCellT table i j
    | i < 0 || j < 0 || i > 2 || j > 2 = False
    | value (table !! i !! j) == -1 = True
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


-- If someone won the game the sign X or O is returned
returnWinner :: Game -> [Char]
returnWinner markedGame 
    | winnerDiagonals == "O" || winnerDiagonals == "X" = winnerDiagonals
    | winnerColumns == "O" || winnerColumns == "X" = winnerColumns
    | winnerLines == "O" || winnerLines == "X" = winnerLines 
    | checkTie (table markedGame) = "T"
    | otherwise = ""
    where 
        winnerDiagonals = checkDiagonals $ table markedGame
        winnerColumns = checkColumns $ table markedGame
        winnerLines = checkRows $ table markedGame

checkTie :: [[Cell]] -> Bool
checkTie table
    | unTakenCells == 0 = True
    | otherwise = False
    where 
        unTakenCells = addUpUnTakens (table !! 0) + addUpUnTakens (table !! 1) + addUpUnTakens (table !! 2)

addUpUnTakens :: [Cell] -> Int
addUpUnTakens row = length [ cell | cell <- row, value cell == -1 ]

-- The functions below checks whether there is a winner or not
checkDiagonals :: [[Cell]] -> [Char]
checkDiagonals table
    | firstDiagonal == [0, 0, 0] = "O"
    | secondDiagonal == [1, 1, 1] = "X"
    | firstDiagonal == [0, 0, 0] = "O"
    | secondDiagonal == [1, 1, 1] = "X"
    | otherwise = ""
    where
        firstDiagonal = [value (table !! 0 !! 0), value (table !! 1 !! 1), value  (table !! 2 !! 2)]
        secondDiagonal = [value (table !! 0 !! 0), value (table !! 1 !! 1), value (table !! 2 !! 2)]

checkColumns :: [[Cell]] -> [Char]
checkColumns table
    | firstColumn == [0, 0, 0] = "O"
    | secondColumn == [0, 0, 0] = "O"
    | thirdColumn == [0, 0, 0] = "O"
    | firstColumn == [1, 1, 1] = "X"
    | secondColumn == [1, 1, 1] = "X"
    | thirdColumn == [1, 1, 1] = "X"
    | otherwise = ""
    where 
        firstColumn = [value (table !! 0 !! 0), value (table !! 1 !! 0), value (table !! 2 !! 0)]
        secondColumn = [value (table !! 0 !! 1), value (table !! 1 !! 1), value (table !! 2 !! 1)]
        thirdColumn = [value (table !! 0 !! 2), value (table !! 1 !! 2), value (table !! 2 !! 2)]

checkRows :: [[Cell]] -> [Char]
checkRows table 
    | null table = ""
    | winner == "O" || winner == "X" = winner
    | otherwise = checkRows $ tail table
    where
        winner = checkRow $ head table 

checkRow :: [Cell] -> [Char]
checkRow line 
    | val == [0, 0, 0] = "O" 
    | val == [1, 1, 1] = "X"
    | otherwise = ""
    where
        val = [ value cell | cell <- line ]

-- The functions below are printing the state of the table
printCurrentStateOfGame :: Game -> IO ()
printCurrentStateOfGame game = do
    let header = makeHeaderStr 0
    let str = makeTableStr (table game) 0
    putStrLn $ "\t" ++ header ++ "\n" ++ str

makeHeaderStr :: (Show a, Num a, Eq a) => a -> [Char]
makeHeaderStr x
    | x == 3 = ""
    | otherwise = show(x) ++ "\t" ++  makeHeaderStr (x+1) 

makeTableStr :: (Show a, Num a, Eq a) => [[Cell]] -> a -> [Char]
makeTableStr table i
    | null table = ""
    | otherwise = makeRowStr (head table) i ++ "\n" ++ makeTableStr (tail table) (i+1) 

makeRowStr :: (Show a, Num a, Eq a) => [Cell] -> a -> [Char]
makeRowStr row i
    | i /= -1 = show(i) ++ " |\t"  ++ makeRowStr row (-1)
    | null row = ""
    | otherwise = show(value (head row))  ++ "\t" ++ makeRowStr (tail row) i