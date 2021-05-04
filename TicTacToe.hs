import GameWithAI
import GameWithTwo

----- Main -----

--- How to start the game: mainTicTacToe ---
mainTicTacToe = do
    putStrLn "Welcome!"
    printOptions
    getSelection
    
printOptions :: IO ()
printOptions = do
    putStrLn "1. Play with AI"
    putStrLn "2. Play with your friend"
    putStrLn "3. Exit"

-- This function gets the input from the user
getSelection :: IO ()
getSelection = do
    putStr "Select: "
    selection <- getLine
    sortOutSelection (read selection::Int)

sortOutSelection :: Int -> IO ()
sortOutSelection selection
    | selection == 1 = gameWithAI
    | selection == 2 = gameWithTwoPlayers
    | selection == 3 = gameExit
    | otherwise = do
        putStrLn "Try again!"
        getSelection

gameExit :: IO ()
gameExit = putStrLn "Thanks for the game!\nGoodbye!"