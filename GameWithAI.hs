--- Ez a modul tartalmazza az AI illetve az AI elleni játék logikáját ---
--- Az AI gondolkodásához a MINIMAX algoritmust használtam ---
module GameWithAI (
    gameWithAI,
    startGame,
    printSelection
)where

import Game -- Importolom a játék logikáját illetve a külöböző adatstrukturákat --

-- Az alábbi függvény a modul 'main'-je --
gameWithAI :: IO ()
gameWithAI = do
    let game = makeGame -- Itt történik egy új játék, tábla inicializálása majd meghívódik maga a játék logikája --
    startGame game

-- Az alábbi függvény kezeli a játékot, illetve annak menetét --
startGame :: Game -> IO ()
startGame game = do
    printCurrentStateOfGame game -- Előszőr kiíratom a kurens játékot --

    printSelection -- Majd kiírom melyik játékos következik --
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
    let markedGame = if inputCheck then rewriteGame (markX (table game) 0 i j) 0 else rewriteGame (markO (table game) 0 (-1) (-1)) (playerMove game)
    
    -- Ellenőrzőm, ha van egy nyertes --
    let winner = returnWinner markedGame

    -- Ha van nyertes vagy döntetlen a megfelelő kimenetel kiíródik, ha nem akkor az AI megjelöli a megfelelő cellat majd újra meghívódik --
    if winner == "" then markAIMove markedGame else (if winner == "T" then do
        printEndGame markedGame
        putStrLn "Tie" 
        else do
        printEndGame markedGame
        putStrLn $ "Winner: " ++ winner )

-- Itt csak az X játékostól kell adatot bekérni mivel az O játkos az az AI --
printSelection :: IO ()
printSelection = putStrLn "Player X turn:"

-- Az alábbi függvény hívódik meg, hogy az AI jelöljön meg egy cellát --
markAIMove :: Game -> IO ()
markAIMove game = do
    let bestMove = minimax (table game) (0, 0) True -- 'bestMove' egy tuple: az első eleme a maximum/minimum érték, a második illetve harmadik elem a koordinátákat adja vissza amit az AI akar megjelölni --

    -- Itt jelöli meg az AI a celláját --
    let markedGame = rewriteGame (markO (table game) 0 (getSecond bestMove) (getThird bestMove)) 0

    -- Majd megint le kell ellenőrizni, ha van-e nyertes --
    let winner = returnWinner markedGame

    -- Ha van nyertes vagy döntetlen a megfelelő kimenetel kiíródik, ha nem akkor újra meghívódik startGame függvény --
    if winner == "" then startGame markedGame else printWinner markedGame winner
    
-- Ez a függvény tartalmazza a minimax algoritmus logikáját --
-- Paraméterek: currens tábla, egy segéd változó, illetve egy flag ami megmondja, hogy a jelenlegi meghívásnál melyik játékos választ --
minimax :: (Ord b, Num b) => [[Cell]] -> (Int, Int) -> Bool -> (b, Int, Int) -- A visszatérített érték egy tuple, első eleme: max/min érték, második illetve harmadik elem: a kiválasztott cella koordinátái --
minimax table cell isMaximizing
    | winner == "X" = (10, fst cell, snd cell) -- 3 lehetséges kimenet van: O nyer -> -10 pont, X nyer -> 10 pont, Döntetlen -> 0 pont  --
    | winner == "O" = (-10, fst cell, snd cell)
    | winner == "T" = (0, fst cell, snd cell)
    -- Az alábbi két sor választa ki a megfelő kimenetelt, ha AI kell válasszon akkor ő maximumot választ, ha nem akkor feltételezzük, hogy a játékos az AI számára a legrosszabb kimenetelt fogja választani --
    -- Tehát megkeresem a maximum értéket a listából, majd filterelem a listát illetve kiválasztom az első elemet a filterelés után --
    | isMaximizing == True = (filter ((==) (maximum $ map getFirst maxList) . getFirst) maxList) !! 0 
    | otherwise = (filter ((==) (minimum $ map getFirst minList) . getFirst ) minList) !! 0
    where
        winner = returnWinner (rewriteGame table 0) -- Ebbe a váltzóba mentem el a nyertest, ha van. Ha nincs akkor üres sztring tér vissza --
        -- Alább felépítek egy kereső fát az összes lehetséges kimenetellelt. A fa alján a levelek értéket kapnak majd azok lesznek összehasonlítva --
        -- Mivel minden kimenet felépül az AI első lépése lassabban jön meg -- 
        maxList = [ minimax (markO table 0 (fst cell) (snd cell)) cell False | cell <- [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)], isFreeCell table (fst cell) (snd cell) ]
        minList = [ minimax (markX table 0 (fst cell) (snd cell)) cell True | cell <- [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)], isFreeCell table (fst cell) (snd cell) ]

-- Az alábbi függvények egy 3 elemü tuplet kapnak paraméterül majd a nevüknek megfelelő értéket térítik vissza --
-- Segéd függvények --
getFirst :: (t, t1, t2) -> t
getFirst (first,_,_) = first

getSecond :: (t, t1, t2) -> t1
getSecond (_,second,_) = second

getThird :: (t, t1, t2) -> t2
getThird (_,_,third) = third

-- Ez a függvény kezeli le a nyertest, illetve írja ki a végső táblát
printWinner :: Game -> [Char] -> IO ()
printWinner game winner  = do
    if winner == "T" then do
        printEndGame game -- Ha lejárt a játék újra kiírom a táblát majd a nyertest  --
        putStrLn "It is a: Tie\n"
        else do
        printEndGame game -- Ha lejárt a játék újra kiírom a táblát majd a nyertest  --
        putStrLn ( "Winner: " ++ winner ++ "\n")