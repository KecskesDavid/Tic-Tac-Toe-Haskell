--- The Game is structured in 4 modules ---
--- TicTacToe (main), Game, GameWithAi, GameWithTwo ---
--- This module contains the main IO functionalities of the game like: menu, selection, exit etc. ---

import GameWithAI
import GameWithTwo

----- Main -----
--- How to start the game: mainTicTacToe ---
-- This function is the entry point of the application --
mainTicTacToe :: IO ()
mainTicTacToe = do
    putStrLn "Welcome!"
    printOptions
    getSelection
    
-- Prints the menu of the application, 3 options are listed --
printOptions :: IO ()
printOptions = do
    putStrLn "1. Play with AI" -- For playing with ai --
    putStrLn "2. Play with your friend" -- For playing multiplayer --
    putStrLn "3. Exit" -- For exiting the game --

-- Gets the input from the user --
getSelection :: IO ()
getSelection = do
    putStr "Select: "
    selection <- getLine 
    sortOutSelection (read selection::Int) -- Calls the function for sorting out the input of the user --

-- Sorts out the input of the user --
sortOutSelection :: Int -> IO ()
sortOutSelection selection -- this is done by pattern matching (case) --
    | selection == 1 = gameWithAI -- Calls a function from the GameWithAi module --
    | selection == 2 = gameWithTwoPlayers -- Calls a function from the GameWithTwo module --
    | selection == 3 = gameExit -- Calls a function for ending the game --
    | otherwise = do -- Otherwise, there is recursive call for the input function --
        putStrLn "Try again!"
        getSelection

-- This is the endpoint of the application --
gameExit :: IO ()
gameExit = putStrLn "Thanks for the game!\nGoodbye!"