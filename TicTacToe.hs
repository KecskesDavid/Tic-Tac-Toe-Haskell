--- A projekt 4 moduleból épül fel ---
--- TicTacToe (ez a main module), Game, GameWithAi, GameWithTwo ---
--- Ez a modul tartalmazza a maint illetve az IO müveletek, mint pl. : menü, szelekció, kilépés stb. ---
module TicTacToe (
    printOptions
)where

import GameWithAI
import GameWithTwo

----- Main -----
--- A játék indításához a következő függvényt kell meghívni: mainTicTacToe ---
-- Ez az applikáció entry pointja --
mainTicTacToe :: IO ()
mainTicTacToe = do
    putStrLn "Welcome!"
    printOptions
    getSelection
    
-- Prints the menu of the application, 3 options are listed --
-- Kiírja a menüt, 3 opciót tud kiválasztani a játékos --
printOptions :: IO ()
printOptions = do
    putStrLn "1. Play with AI" -- Az AI-al való játékhoz --
    putStrLn "2. Play with your friend" -- Multiplayer esetén --
    putStrLn "3. Exit" -- Kilépés --

-- Az alábbi függvény kéri be a user választását a menüből --
getSelection :: IO ()
getSelection = do
    putStr "Select: "
    selection <- getLine 
    sortOutSelection (read selection::Int) -- Majd meghívja azt a függvényt amelyik továbbítja a kérést --

-- Ez a függvény felelős azért, hogy a user által beírt adat továbbitódjon --
sortOutSelection :: Int -> IO ()
sortOutSelection selection -- Pattern matching-el oldottam ezt meg, hasonlóan egy case-hez --
    | selection == 1 = gameWithAI -- A GameWithAi modul main function-je hívódik meg --
    | selection == 2 = gameWithTwoPlayers -- A GameWithTwo modul main function-je hívódik meg --
    | selection == 3 = gameExit -- Egy olyan függvényt hív meg amelyikkel kilépünk a játékból --
    | otherwise = do -- Ha rossz a bemenet az előző függvény meghívódik megint --
        putStrLn "Try again!"
        getSelection

-- Applikáció endpointja --
gameExit :: IO ()
gameExit = putStrLn "Thanks for the game!\nGoodbye!"