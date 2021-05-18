--- This module contains the logic for the multiplayer game ---
module GameWithTwo (
    gameWithTwoPlayers,
    startGame,
    printSelection
)where

import Game -- Importing the game logic --

-- Entry point for the multiplayer game --
gameWithTwoPlayers :: IO ()
gameWithTwoPlayers = do
    let game = makeGame -- Making a game entity --
    startGame game

-- Game logic for multiplayer --
-- Recursive function, prints out current state, gets input, checks for winner then repeat --
startGame :: Game -> IO ()
startGame game = do
    printCurrentStateOfGame game  -- The current state of the game is printed --

    printSelection game -- Printing player turn --
    -- Gets the input from user --
    putStr "i: "
    tempi <- getLine
    let i = read tempi::Int
    -- Gets the input from user --
    putStr "j: "
    tempj <- getLine
    let j = read tempj::Int

    -- Making a new game with the marked cell if it the cell is valid and free --
    let markedGame = if isFreeCell game i j then (if playerMove game == 1 then rewriteGame (markX (table game) 0 i j) 0 else rewriteGame (markO (table game) 0 i j) 1) else rewriteGame (markO (table game) 0 (-1) (-1)) (playerMove game)
    
    -- Checking for winner --
    let winner = returnWinner markedGame

    -- In case if the game ended then the appropiate end is printed, otherwise the 'startGame' is called with the new marked game --
    if winner == "" then startGame markedGame else if winner == "T" then putStrLn "Tie" else putStrLn ( "winner: " ++ winner )

-- Printing which players turn comes --
printSelection :: Game -> IO ()
printSelection game = do
    let player = playerMove game
    putStr "Player "
    if player == 0 then putStr "O" else putStr "X"
    putStrLn " turn:"