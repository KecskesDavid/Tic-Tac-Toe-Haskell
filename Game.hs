--- This module contains the logic behind the game ---
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

--- A 'Cell' of a table, contains a value ---
data Cell = Cell {
    value :: Int -- -1, 0, 1 --
}deriving (Show) 

--- A 'Game' is a data structure that contains a table and the current player move ---
data Game = Game {
    table :: [[Cell]], -- Contains the current state of the game, 3x3 table -- 
    playerMove :: Int -- 0 or 1 depending on the current player, 0 -> O player, 1 -> X player --
}deriving (Show) 

-- Makes a game entity, with a 3x3 table and 1 (X) as starting player --
makeGame :: Game
makeGame = Game [[makeCell, makeCell, makeCell], [makeCell, makeCell, makeCell], [makeCell, makeCell, makeCell]] 1

-- Recreats a game with a new, marked cell --
rewriteGame :: [[Cell]] -> Int -> Game
rewriteGame table playerMove = Game table playerMove

-- Makes a cell entity with a default value as -1 --
makeCell :: Cell
makeCell = Cell (-1)

-- When marking cells I needed functions that make cell entities with either 1 or 0 as starting value --
makeCellX :: Cell
makeCellX = Cell 1 -- Returns a cell with 1 as value --

makeCellO :: Cell
makeCellO = Cell 0 -- Returns a cell with 0 as value --


-- Checks for free cells --
isFreeCell :: Game -> Int -> Int -> Bool
isFreeCell game i j
    | i < 0 || j < 0 || i > 2 || j > 2 = False -- Only valid cells are tested --
    | value ((table game) !! i !! j) == -1 = True -- If the value of the ith and jth cell is not -1 then return True, because that cell is taken --
    | otherwise = False

-- Does the same as the function before but as input it gets a table --
isFreeCellT :: [[Cell]] -> Int -> Int -> Bool
isFreeCellT table i j
    | i < 0 || j < 0 || i > 2 || j > 2 = False
    | value (table !! i !! j) == -1 = True
    | otherwise = False

-- Marks a X on the table then returns it --
-- curj should be 0 --
markX :: (Num a1, Num a, Eq a1, Eq a) =>[[Cell]] -> a1 -> a1 -> a -> [[Cell]]
markX table curi i j -- i,j are the input of the user/ai --
    | curi == 3 = []
    | otherwise = makeRowX (head table) curi 0 i j : markX (tail table) (curi+1) i j -- Going through every [Cell] --

-- Marks a X in a row then returns it --
makeRowX :: (Num a, Eq a1, Eq a) => [Cell] -> a1 -> a -> a1 -> a -> [Cell]
makeRowX row curi curj i j
    | curj == 3 = []
    | curj == j && curi == i = makeCellX : makeRowX (tail row) curi (curj+1) i j -- If the i, j match the curi, curj then that cell is marked with X --
    | otherwise = (head row) : makeRowX (tail row) curi (curj+1) i j

-- Marks a O on the table then returns it
-- curi should be 0
markO :: (Num a1, Num a, Eq a1, Eq a) =>[[Cell]] -> a1 -> a1 -> a -> [[Cell]]
markO table curi i j -- i,j are the input of the user/ai --
    | curi == 3 = []
    | otherwise = makeRowO (head table) curi 0 i j : markO (tail table) (curi+1) i j -- Going through every [Cell] --

-- Marks a O in a row then returns it --
makeRowO :: (Num a, Eq a1, Eq a) => [Cell] -> a1 -> a -> a1 -> a -> [Cell]
makeRowO row curi curj i j
    | curj == 3 = []
    | curj == j && curi == i = makeCellO : makeRowO (tail row) curi (curj+1) i j -- If the i, j match the curi, curj then that cell is marked with O --
    | otherwise = (head row) : makeRowO (tail row) curi (curj+1) i j

-- Goes through every possible winning scenario, then if someone have won then it returns it --
-- There are 4 cases of return: 'X' -> X is the winner, 'O' -> O is the winner, 'T' -> It is a tie, '' -> No one won  --
returnWinner :: Game -> [Char]
returnWinner markedGame 
    | winnerDiagonals == "O" || winnerDiagonals == "X" = winnerDiagonals -- Checking diagonals --
    | winnerColumns == "O" || winnerColumns == "X" = winnerColumns -- Checking columns --
    | winnerRows == "O" || winnerRows == "X" = winnerRows -- Checking rows --
    | checkTie (table markedGame) = "T"
    | otherwise = ""
    where 
        winnerDiagonals = checkDiagonals $ table markedGame
        winnerColumns = checkColumns $ table markedGame
        winnerRows = checkRows $ table markedGame

-- Checks for a tie --
checkTie :: [[Cell]] -> Bool
checkTie table
    | unTakenCells == 0 = True
    | otherwise = False
    where 
        unTakenCells = addUpUnTakens (table !! 0) + addUpUnTakens (table !! 1) + addUpUnTakens (table !! 2) -- Adding up every taken cell --

-- This function is a auxiliar function, adds up every taken cells --
addUpUnTakens :: [Cell] -> Int
addUpUnTakens row = length [ cell | cell <- row, value cell == -1 ]

-- Checks for a winner on every diagonal --
checkDiagonals :: [[Cell]] -> [Char]
checkDiagonals table
    | firstDiagonal == [0, 0, 0] = "O" -- Every possible cases --
    | secondDiagonal == [1, 1, 1] = "X"
    | firstDiagonal == [0, 0, 0] = "O"
    | secondDiagonal == [1, 1, 1] = "X"
    | otherwise = ""
    where
        firstDiagonal = [value (table !! 0 !! 0), value (table !! 1 !! 1), value  (table !! 2 !! 2)]
        secondDiagonal = [value (table !! 0 !! 2), value (table !! 1 !! 1), value (table !! 0 !! 2)]

-- Checks for a winner on every column --
checkColumns :: [[Cell]] -> [Char]
checkColumns table
    | firstColumn == [0, 0, 0] = "O" -- Every possible cases --
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

-- Checks for a winner on every row --
checkRows :: [[Cell]] -> [Char]
checkRows table 
    | null table = ""
    | winner == "O" || winner == "X" = winner
    | otherwise = checkRows $ tail table -- Going through row by row and returning the winner
    where
        winner = checkRow $ head table 

-- Checks a single line for a winner --
checkRow :: [Cell] -> [Char]
checkRow line 
    | val == [0, 0, 0] = "O"  -- Every possible cases --
    | val == [1, 1, 1] = "X"
    | otherwise = ""
    where
        val = [ value cell | cell <- line ]

--- The functions below are printing the state of the table ---
-- This function is called for printing the current state of the game, appends everything together then prints it --
printCurrentStateOfGame :: Game -> IO ()
printCurrentStateOfGame game = do
    let header = makeHeaderStr 0
    let str = makeTableStr (table game) 0
    putStrLn $ "\t" ++ header ++ "\n" ++ str

--- The functions named with 'make...Str' are for making custom string then returning them ---

-- Makes a header with the coordinates from 0 to 2 (these are the coordinates that the user should write as input) --
makeHeaderStr :: (Show a, Num a, Eq a) => a -> [Char]
makeHeaderStr x
    | x == 3 = ""
    | otherwise = show(x) ++ "\t" ++  makeHeaderStr (x+1) 

-- This function appends every string that the 'makeRowStr' returns -- 
makeTableStr :: (Show a, Num a, Eq a) => [[Cell]] -> a -> [Char]
makeTableStr table i
    | null table = ""
    | otherwise = makeRowStr (head table) i ++ "\n" ++ makeTableStr (tail table) (i+1) 

-- Makes a row from a single [Cell] structure --
makeRowStr :: (Show a, Num a, Eq a) => [Cell] -> a -> [Char]
makeRowStr row i
    | i /= -1 = show(i) ++ " |\t"  ++ makeRowStr row (-1)
    | null row = ""
    | otherwise = show(value (head row))  ++ "\t" ++ makeRowStr (tail row) i