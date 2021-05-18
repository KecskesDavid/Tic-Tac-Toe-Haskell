module GameWithAI (
    gameWithAI,
    startGame,
    printSelection
)where

import Game

gameWithAI :: IO ()
gameWithAI = do
    let game = makeGame
    startGame game

startGame :: Game -> IO ()
startGame game = do
    printCurrentStateOfGame game
    printSelection
    
    putStr "i: "
    tempi <- getLine
    let i = read tempi::Int

    putStr "j: "
    tempj <- getLine
    let j = read tempj::Int

    let markedGame = if isFreeCell game i j then rewriteGame (markX (table game) 0 i j) 0 else rewriteGame (markO (table game) 0 (-1) (-1)) (playerMove game)
    
    let winner = returnWinner markedGame

    if winner == "" then markAIMove markedGame else (if winner == "T" then putStrLn "Tie" else putStrLn $ "winner: " ++ winner )

printSelection :: IO ()
printSelection = putStrLn "Player X turn:"

markAIMove :: Game -> IO ()
markAIMove game = do
    let bestMove = minimax (table game) (0, 0) True

    let markedGame = rewriteGame (markO (table game) 0 (getSecond bestMove) (getThird bestMove)) 0

    let winner = returnWinner markedGame

    if winner == "" then startGame markedGame else (if winner == "T" then putStrLn "Tie" else putStrLn $ "winner: , AI " ++ winner)

minimax :: (Ord b, Num b) => [[Cell]] -> (Int, Int) -> Bool -> (b, Int, Int)
minimax table cell isMaximizing
    | winner == "X" = (10, fst cell, snd cell)
    | winner == "O" = (-10, fst cell, snd cell)
    | winner == "T" = (0, fst cell, snd cell) -- 0 0, 0 2
    | isMaximizing == True = (filter ((==) (maximum $ map getFirst maxList) . getFirst) maxList) !! 0
    | otherwise = (filter ((==) (minimum $ map getFirst minList) . getFirst ) minList) !! 0
    where
        winner = returnWinner (rewriteGame table 0)
        maxList = [ minimax (markO table 0 (fst cell) (snd cell)) cell False | cell <- [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)], isFreeCellT table (fst cell) (snd cell) ]
        minList = [ minimax (markX table 0 (fst cell) (snd cell)) cell True | cell <- [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)], isFreeCellT table (fst cell) (snd cell) ]

getFirst :: (t, t1, t2) -> t
getFirst (first,_,_) = first

getSecond :: (t, t1, t2) -> t1
getSecond (_,second,_) = second

getThird :: (t, t1, t2) -> t2
getThird (_,_,third) = third

-- markMove table curi (i, j, curMax) 
--     | curi == 3 = rewriteGame (markO table 0 i j) 0
--     | otherwise = markMove table (curi+1) (makeMoveRow table curi 0 (-100) i j) 

-- -- curi curj for updating the set, i j the current set items
-- makeMoveRow table curi curj curMax i j
--     | curj == 3 = (i, j, curMax)
--     | isFreeCellT table curi curj = if minimaxval > curMax then makeMoveRow table curi (curj+1) minimaxval curi curj else makeMoveRow table curi (curj+1) curMax i j 
--     | otherwise = makeMoveRow table curi (curj+1) curMax i j 
--     where 
--         minimaxval = minimax (markO table 0 curi curj) False 0

-- minimax table isMaximizing score
--     | winner == "X" = 1
--     | winner == "O" = (-1)
--     | winner == "T" = 0
--     | otherwise = auxFindMinMax table isMaximizing
--     -- | isMaximizing == True = (minimax (findNextMove table 0 ((-1), (-1))) False score)
--     -- | isMaximizing == False = (minimax (findNextMove table 0 ((-1), (-1))) True score)
--     where
--         winner = returnWinner (rewriteGame table 0)

-- auxFindMinMax table isMaximizing = do
--     if isMaximizing == True then do
--         let bestScore = -10
--         let score = minimax (findNextMove table 0 ((-1), (-1))) False score
--         if score > bestScore then score else bestScore
--     else do
--         let bestScore = 10
--         let score = minimax (findNextMove table 0 ((-1), (-1))) True score
--         if score < bestScore then score else bestScore 

-- findNextMove table curi (i, j)
--     | curi == 3 = markO table 0 i j
--     | otherwise = findNextMove table (curi+1) (findNextMoveRow table curi 0) 

-- -- curi curj for updating the set, i j the current set items
-- findNextMoveRow table curi curj
--     | curj == 3 = ((-1), (-1))
--     | isFreeCellT table curi curj = (curi, curj)
--     | otherwise = findNextMoveRow table curi (curj+1)