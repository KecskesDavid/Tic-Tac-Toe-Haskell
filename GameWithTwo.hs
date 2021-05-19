--- Ez a modul tartalmazza a multiplazer logikáját ---
module GameWithTwo (
    gameWithTwoPlayers,
    startGame,
    printSelection
)where

import Game -- Importolom a játék logikáját illetve a külöböző adatstrukturákat --

-- Az alábbi függvény a modul 'main'-je --
gameWithTwoPlayers :: IO ()
gameWithTwoPlayers = do
    let game = makeGame -- Itt történik egy új játék, tábla inicializálása majd meghívódik maga a játék logikája --
    startGame game

-- Az alábbi függvény kezeli a játékot, illetve annak menetét --
startGame :: Game -> IO ()
startGame game = do
    printCurrentStateOfGame game  -- Előszőr kiíratom a kurens játékot --

    printSelection game -- Majd kiírom melyik játékos következik --
    -- A user a következő opciókból választhat [0, 1, 2] --
    -- Első koordináta bekérése --
    putStr "i: "
    tempi <- getLine
    let i = read tempi::Int
    -- Második koordináta bekérése --
    putStr "j: "
    tempj <- getLine
    let j = read tempj::Int

    -- Ha a beadott koordináták helyesek illetve szabad a cella, újra megépítem a táblát és megjelölöm az illető cellát --
    let markedGame = if isFreeCell (table game) i j then (if playerMove game == 1 then rewriteGame (markX (table game) 0 i j) 0 else rewriteGame (markO (table game) 0 i j) 1) else rewriteGame (markO (table game) 0 (-1) (-1)) (playerMove game)
    
    -- Ellenőrzőm, ha van egy nyertes --
    let winner = returnWinner markedGame

    -- Ha van nyertes vagy döntetlen a megfelelő kimenetel kiíródik, ha nem akkor az AI megjelöli a megfelelő cellat majd újra meghívódik --
    if winner == "" then startGame markedGame else if winner == "T" then do
        printEndGame markedGame
        putStrLn "Tie"
        else do
        printEndGame markedGame 
        putStrLn ( "Winner: " ++ winner )

-- Kiírom, hogy melyik játékos következik --
printSelection :: Game -> IO ()
printSelection game = do
    let player = playerMove game
    putStr $ "Player " ++ (if player == 0 then "O" else "X")
    putStrLn " turn:"