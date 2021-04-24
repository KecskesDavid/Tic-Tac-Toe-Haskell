module GameWithTwo (
    gameWithTwoPlayers,
    startGame,
    printCurrentStateOfGame,
    makeHeaderStr,
    makeTableStr,
    makeRowStr,
    printSelection
)where


import Game

gameWithTwoPlayers :: IO b
gameWithTwoPlayers = do
    let game = makeGame
    startGame game

startGame :: Game -> IO b
startGame game = do
    printCurrentStateOfGame game
    printSelection game
    
    putStr "i: "
    tempi <- getLine
    let i = read tempi::Int

    putStr "i: "
    tempj <- getLine
    let j = read tempj::Int

    let markedGame = if isFreeCell game i j then (if playerMove game == 1 then rewriteGame (markX (table game) 0 i j) 0 else rewriteGame (markO (table game) 0 i j) 1) else rewriteGame (markO (table game) 0 (-1) (-1)) (playerMove game)
    
    startGame markedGame

printCurrentStateOfGame :: Game -> IO ()
printCurrentStateOfGame game = do
    let header = makeHeaderStr 0
    let str = makeTableStr (table game) 0
    putStrLn $ "\t" ++ header ++ "\n" ++ str

makeHeaderStr :: (Show a, Num a, Eq a) => a -> [Char]
makeHeaderStr x
    | x == 3 = ""
    | otherwise = show(x) ++ "\t" ++  makeHeaderStr (x+1) 

makeTableStr :: (Show a, Num a, Eq a) => [[Cell]] -> a -> [Char]
makeTableStr table i
    | null table = ""
    | otherwise = makeRowStr (head table) i ++ "\n" ++ makeTableStr (tail table) (i+1) 

makeRowStr :: (Show a, Num a, Eq a) => [Cell] -> a -> [Char]
makeRowStr row i
    | i /= -1 = show(i) ++ " |\t"  ++ makeRowStr row (-1)
    | null row = ""
    | otherwise = show(value (head row))  ++ "\t" ++ makeRowStr (tail row) i

printSelection :: Game -> IO ()
printSelection game = do
    let player = playerMove game
    putStr "Player "
    if player == 0 then putStr "O" else putStr "X"
    putStrLn " turn:"