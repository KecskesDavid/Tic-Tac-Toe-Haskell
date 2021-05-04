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

    if winner == "" then markAIMove markedGame else putStrLn ( "winner: " ++ winner)

printSelection :: IO ()
printSelection = putStrLn "Player X turn:"

markAIMove game = do
    let markedGame = markMove (table game) 0 (-1, -1, -1)
    let winner = returnWinner markedGame

    if winner == "" then startGame markedGame else putStrLn ( "winner: , AI" ++ winner)

markMove table curi (i, j, curMax) 
    | curi == 3 = rewriteGame (markO table 0 i j) 0
    | otherwise = markMove table (curi+1) (makeMoveRow table curi 0 (-1) i j) 

-- curi curj for updating the set, i j the current set items
makeMoveRow table curi curj curMax i j
    | curj == 3 = (i, j, curMax)
    | isFreeCellT table curi curj = if minimaxval > curMax then makeMoveRow table curi (curj+1) minimaxval curi curj else makeMoveRow table curi (curj+1) curMax i j 
    | otherwise = makeMoveRow table curi (curj+1) curMax i j 
    where 
        minimaxval = minimax table curi curj
    
minimax table curi curj = curj