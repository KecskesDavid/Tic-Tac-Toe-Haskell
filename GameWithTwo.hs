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

    -- Leellenőrzőm, hogy a megadott koordináták helyesek, illetve szabad-e az illető cella -- 
    let inputCheck = isFreeCell (table game) i j

    -- Ha nem, egy üzenet fog kiírodni, illetve a játék a jelenlegi eredménnyel újra fog indulni --
    if inputCheck == False then do
        putStrLn "\nWrong input, try again: "
        startGame game
        else pure ()

    -- Csak akkor ér ide a függvény, ha a koordináták helyesek illetve szabad a cella. Ekkor újra megépítem a táblát és megjelölöm az illető cellát --
    let markedGame = if playerMove game == 1 then rewriteGame (markX (table game) 0 i j) 0 else rewriteGame (markO (table game) 0 i j) 1

    -- Ellenőrzőm, ha van egy nyertes --
    let winner = returnWinner markedGame

    -- Ha van nyertes vagy döntetlen a megfelelő kimenetel kiíródik, ha nem akkor az AI megjelöli a megfelelő cellat majd újra meghívódik --
    if winner == "" then startGame markedGame else printWinner markedGame winner

-- Kiírom, hogy melyik játékos következik --
printSelection :: Game -> IO ()
printSelection game = do
    let player = playerMove game
    putStr $ "Player " ++ (if player == 0 then "O" else "X")
    putStrLn " turn:"

-- Ez a függvény kezeli le a nyertest, illetve írja ki a végső táblát
printWinner :: Game -> [Char] -> IO ()
printWinner game winner  = do
    if winner == "T" then do
        printEndGame game -- Ha lejárt a játék újra kiírom a táblát majd a nyertest  --
        putStrLn "It is a: Tie\n"
        else do
        printEndGame game -- Ha lejárt a játék újra kiírom a táblát majd a nyertest  --
        putStrLn ( "Winner: " ++ winner ++ "\n")