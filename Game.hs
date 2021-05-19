--- Ez a modul tartalmazza a játék logikáját, illetve különböző IO müveletek ---
module Game (
    Cell (..),
    Game (..),
    makeGame,
    rewriteGame,
    makeCell,
    makeCellX,
    makeCellO,
    isFreeCell,
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
    printEndGame,
    makeHeaderStr,
    makeTableStr,
    makeRowStr
)where

import Data.List

--- A 'Cell' adatstruktúra egy elem a táblából, ennek értékei lehetnek -1, 0, 1---
data Cell = Cell {
    value :: Int -- -1, 0, 1 --
}deriving (Show) 

--- A 'Game' adatstruktura tartalmaz egy táblát ami egy 3x3as Cellekből álló mátrix. Illetve tartalmaz egy flaget, playerMove, ami megmutatja melyik jatékos következik ---
data Game = Game {
    table :: [[Cell]], -- Kurens tábla, 3x3as mátrix -- 
    playerMove :: Int -- 0 -> 'O' játékos, 1 -> 'X' játékos --
}deriving (Show) 

-- Egy játékot inicializál: 3x3-as tábla illetve, 1-es vagyis X játékossal mint kezdés --
makeGame :: Game
makeGame = Game [[makeCell, makeCell, makeCell], [makeCell, makeCell, makeCell], [makeCell, makeCell, makeCell]] 1

-- Újraírja a játékot a megadott táblával illetve játékossal --
rewriteGame :: [[Cell]] -> Int -> Game
rewriteGame table playerMove = Game table playerMove

-- Egy Cell példányt inicializál, default value a -1 --
makeCell :: Cell
makeCell = Cell (-1)

-- Mikor valaki megjelöl egy cellát akkor a meglévő helyett újat kellett létrehozzak ezért a következő két function ezt a feladatot látja el --
makeCellX :: Cell
makeCellX = Cell 1 -- Cell példányt hoz létre 1(X) értékkel --

makeCellO :: Cell
makeCellO = Cell 0 -- Cell példányt hoz létre 0(O) értékkel --

-- Leellenőrzi, hogy egy adott cella szabad-e illetve, hogy a megadott koordináták valósak-e --
-- Paraméterek: table - kurens játék táblája, i,j - 2 koordináta illetve egy boolt térít vissza --
isFreeCell :: [[Cell]] -> Int -> Int -> Bool
isFreeCell table i j
    | i < 0 || j < 0 || i > 2 || j > 2 = False -- Csak azokat ellenőrzőm le amelyikek léteznek --
    | value (table !! i !! j) == -1 = True -- Ha az illető érték a mátrixban nem -1 akkor a cella nem üres ezért True-t térítek vissza --
    | otherwise = False

-- Megjelöli a táblát egy X-el a megadott koordináták alapján majd visszetéríti azt --
-- curj 0 kell legyen meghívásnál --
-- Paraméterek: table - kurens tábla, curi - kurens i pozició, az i és j paraméterek a játékos/AI által megadott koordináták --
markX :: (Num a1, Num a, Eq a1, Eq a) =>[[Cell]] -> a1 -> a1 -> a -> [[Cell]]
markX table curi i j
    | curi == 3 = []
    | otherwise = makeRowX (head table) curi 0 i j : markX (tail table) (curi+1) i j -- Itt megyek végig a tábla összes során --

-- Ez a függvény keresi a megadott koordinátákat majd, ha megtalálja megváltoztatja az értéket és visszaadja az illető sort, ha nincs a sorban a keresett visszaadja az eredeti sort változások nélkül --
-- Paraméterek: row - az illető sor ahol keresek, curi,curj - azok a poziciók ahol jelenleg vagyok, i,j - játékos/AI által megadott koordináták --
makeRowX :: (Num a, Eq a1, Eq a) => [Cell] -> a1 -> a -> a1 -> a -> [Cell]
makeRowX row curi curj i j
    | curj == 3 = []
    | curj == j && curi == i = makeCellX : makeRowX (tail row) curi (curj+1) i j -- Ha az i, j egyenlőek a curi-, curj-vel akkor megjelölöm a illető cellát X-el --
    | otherwise = (head row) : makeRowX (tail row) curi (curj+1) i j

-- Megjelöli a táblát egy O-val a megadott koordináták alapján majd visszetéríti azt --
-- curj 0 kell legyen meghívásnál --
-- Paraméterek: table - kurens tábla, curi - kurens i pozició, az i és j paraméterek a játékos/AI által megadott koordináták --
markO :: (Num a1, Num a, Eq a1, Eq a) =>[[Cell]] -> a1 -> a1 -> a -> [[Cell]]
markO table curi i j -- i,j are the input of the user/ai --
    | curi == 3 = []
    | otherwise = makeRowO (head table) curi 0 i j : markO (tail table) (curi+1) i j -- Itt megyek végig a tábla összes során --

-- Ez a függvény keresi a megadott koordinátákat majd, ha megtalálja megváltoztatja az értéket és visszaadja az illető sort, ha nincs a sorban a keresett visszaadja az eredeti sort változások nélkül --
-- Paraméterek: row - az illető sor ahol keresek, curi,curj - azok a poziciók ahol jelenleg vagyok, i,j - játékos/AI által megadott koordináták --
makeRowO :: (Num a, Eq a1, Eq a) => [Cell] -> a1 -> a -> a1 -> a -> [Cell]
makeRowO row curi curj i j
    | curj == 3 = []
    | curj == j && curi == i = makeCellO : makeRowO (tail row) curi (curj+1) i j -- Ha az i, j egyenlőek a curi-, curj-vel akkor megjelölöm a illető cellát X-el --
    | otherwise = (head row) : makeRowO (tail row) curi (curj+1) i j

-- Az alábbi függvények ellenőrzik le, hogy a játéknak van-e nyertese --
-- 4 lehetséges visszatérítési érték van: 'X' -> X a nyertes, 'O' -> O a nyertes, 'T' -> Egyenlő, '' -> Senki sem nyert  --
returnWinner :: Game -> [Char]
returnWinner markedGame 
    | winnerDiagonals == "O" || winnerDiagonals == "X" = winnerDiagonals -- Itt ellenőrzőm le az átlókat --
    | winnerColumns == "O" || winnerColumns == "X" = winnerColumns -- Itt ellenőrzőm le az oszlopokat --
    | winnerRows == "O" || winnerRows == "X" = winnerRows -- Itt ellenőrzőm le az sorokat --
    | checkTie (table markedGame) = "T"
    | otherwise = ""
    where 
        winnerDiagonals = checkDiagonals $ table markedGame
        winnerColumns = checkColumns $ table markedGame
        winnerRows = checkRows $ table markedGame

-- Az alábbi függvény egy mátrixot kap majd abba ellenőrzi le, ha a játék egyenlővel zárult --
checkTie :: [[Cell]] -> Bool
checkTie table
    | unTakenCells == 0 = True
    | otherwise = False
    where 
        unTakenCells = addUpUnTakens (table !! 0) + addUpUnTakens (table !! 1) + addUpUnTakens (table !! 2) -- Összeadom az összes olyan cellát amelyik nincs még elfoglalva --

-- Ez egy segédfüggvény amelyik azokat a cellákat szamolja meg egy sorból amelyikeknek értéke -1 vagyis még nincsenek elfoglalva  --
addUpUnTakens :: [Cell] -> Int
addUpUnTakens row = length [ cell | cell <- row, value cell == -1 ]

-- Az alábbi függvény egy mátrixot kap és ellenőrzi az átlókat ha van nyertes --
checkDiagonals :: [[Cell]] -> [Char]
checkDiagonals table
    | firstDiagonal == [0, 0, 0] = "O"
    | secondDiagonal == [1, 1, 1] = "X"
    | firstDiagonal == [1, 1, 1] = "X"
    | secondDiagonal == [0, 0, 0] = "O"
    | otherwise = ""
    where
        firstDiagonal = [value (table !! 0 !! 0), value (table !! 1 !! 1), value  (table !! 2 !! 2)]
        secondDiagonal = [value (table !! 0 !! 2), value (table !! 1 !! 1), value (table !! 2 !! 0)]

-- Az alábbi függvény egy mátrixot kap és ellenőrzi az oszlopokat ha van nyertes --
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

-- Az alábbi függvény egy mátrixot kap és ellenőrzi az sorokat ha van nyertes --
checkRows :: [[Cell]] -> [Char]
checkRows table 
    | null table = ""
    | winner == "O" || winner == "X" = winner
    | otherwise = checkRows $ tail table
    where
        winner = checkRow $ head table 

-- Az alábbi függvény egy sort kap és ellenőrzi ha van nyertes --
checkRow :: [Cell] -> [Char]
checkRow row 
    | val == [0, 0, 0] = "O"  -- Every possible cases --
    | val == [1, 1, 1] = "X"
    | otherwise = ""
    where
        val = [ value cell | cell <- row ]

--- Az alábbi függvényeknek a kiíratásban van szerepe ---

-- Feldolgozza a kurens játékot, előállítja az összes szükséges karakterláncot majd összefűzi ezeket --
printCurrentStateOfGame :: Game -> IO ()
printCurrentStateOfGame game = do
    let header = makeHeaderStr 0
    let str = makeTableStr (table game) 0
    putStrLn $ "\t" ++ header ++ "\n" ++ str

-- Kiírja mégegszer a játékotmjad kilép
printEndGame :: Game -> IO ()
printEndGame game = do
    putStrLn "\n--- End of the game ---"
    printCurrentStateOfGame game

--- Az alábbi 'make...Str' minta alapján elnevezett függvények egy karakterláncot állítanak elő majd visszatérítik azt ---

-- A tábla header-ét készíti el, i - kurrens pozició --
makeHeaderStr :: (Show a, Num a, Eq a) => a -> [Char]
makeHeaderStr i
    | i == 3 = ""
    | otherwise = show(i) ++ "\t" ++  makeHeaderStr (i+1) 

-- Egy táblát dolgoz fel --
makeTableStr :: (Show a, Num a, Eq a) => [[Cell]] -> a -> [Char]
makeTableStr table i
    | null table = ""
    | otherwise = makeRowStr (head table) i ++ "\n" ++ makeTableStr (tail table) (i+1) -- Mindegyik sort elküldi a 'makeRowStr' ami ezt feldolgozza majd visszatéríti azt --

-- Egy sort dolgoz fel --
makeRowStr :: (Show a, Num a, Eq a) => [Cell] -> a -> [Char]
makeRowStr row i
    | i /= -1 = show(i) ++ " |\t"  ++ makeRowStr row (-1) -- Előszőr egy koordináta csatolódik a karakterlánchoz --
    | null row = ""
    | value (head row) == (-1) = "_\t" ++ makeRowStr (tail row) i -- Majd az összes el nem foglalt mezőnek megfelő '_' karakter --
    | otherwise = (if(value (head row) == 1) then "X" else "O")  ++ "\t" ++ makeRowStr (tail row) i -- Majd az összes elfoglalt mezőnek megfelelő érték --