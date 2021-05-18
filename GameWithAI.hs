--- This module contains the logic for the game against the AI ---
module GameWithAI (
    gameWithAI,
    startGame,
    printSelection
)where

import Game -- Importing the game logic --

-- Entry point for the game against the AI --
gameWithAI :: IO ()
gameWithAI = do
    let game = makeGame
    startGame game

-- Game logic --
-- Recursive function, prints out current state, gets input, checks for winner then repeat --
startGame :: Game -> IO ()
startGame game = do
    printCurrentStateOfGame game -- The current state of the game is printed --

    printSelection -- Printing player turn --
    -- User input should be between [0, 1, 2] --
    -- Gets the input from user --
    putStr "i: "
    tempi <- getLine
    let i = read tempi::Int
    -- Gets the input from user --
    putStr "j: "
    tempj <- getLine
    let j = read tempj::Int

    -- Making a new game with the marked cell if it the cell is valid and free --
    let markedGame = if isFreeCell game i j then rewriteGame (markX (table game) 0 i j) 0 else rewriteGame (markO (table game) 0 (-1) (-1)) (playerMove game)
    
    -- Checking for winner --
    let winner = returnWinner markedGame

    -- In case if the game ended then the appropiate end is printed, otherwise the 'startGame' is called with the new marked game --
    if winner == "" then markAIMove markedGame else (if winner == "T" then putStrLn "Tie" else putStrLn $ "Winner: " ++ winner )

-- Input only comes from one player, the X player --
printSelection :: IO ()
printSelection = putStrLn "Player X turn:"

-- This function is called when tha AI should mark a cell --
markAIMove :: Game -> IO ()
markAIMove game = do
    let bestMove = minimax (table game) (0, 0) True -- 'bestMove' is a (Ord b, Num b) => (b, Int, Int) tuple --

    -- The second and third elements of the tuple are the coordinates that the AI wants to mark --
    -- Marking the game for the AI --
    let markedGame = rewriteGame (markO (table game) 0 (getSecond bestMove) (getThird bestMove)) 0

    -- Checking for winner after AI move--
    let winner = returnWinner markedGame

    -- In case if the game ended then the appropiate end is printed, otherwise the 'startGame' is called with the new marked game --
    if winner == "" then startGame markedGame else (if winner == "T" then putStrLn "Tie" else putStrLn $ "Winner: , AI " ++ winner)

-- This function contains the logic behind the minimax algorithm --
-- The inputs are: current table, an aux cell, and a flag which tells if a max or min should be calculated --
minimax :: (Ord b, Num b) => [[Cell]] -> (Int, Int) -> Bool -> (b, Int, Int) -- The return of the function is a tuple: max value, coordinate i, coordinate j --
minimax table cell isMaximizing
    | winner == "X" = (10, fst cell, snd cell) -- The function has basically 3 outcomes: player won (-10), ai won (10), tie (0) --
    | winner == "O" = (-10, fst cell, snd cell)
    | winner == "T" = (0, fst cell, snd cell)
    | isMaximizing == True = (filter ((==) (maximum $ map getFirst maxList) . getFirst) maxList) !! 0 -- After creating every possible outcome there is applied a filter then the max is returned --
    | otherwise = (filter ((==) (minimum $ map getFirst minList) . getFirst ) minList) !! 0
    where
        winner = returnWinner (rewriteGame table 0) -- Calculates winner if there is one --
        -- Below there is built a tree with every possible outcome (the first step of the AI is slove) -- 
        maxList = [ minimax (markO table 0 (fst cell) (snd cell)) cell False | cell <- [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)], isFreeCellT table (fst cell) (snd cell) ]
        minList = [ minimax (markX table 0 (fst cell) (snd cell)) cell True | cell <- [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)], isFreeCellT table (fst cell) (snd cell) ]

-- The next funtctions returnes the first, third or second element of a tuple --
getFirst :: (t, t1, t2) -> t
getFirst (first,_,_) = first

getSecond :: (t, t1, t2) -> t1
getSecond (_,second,_) = second

getThird :: (t, t1, t2) -> t2
getThird (_,_,third) = third