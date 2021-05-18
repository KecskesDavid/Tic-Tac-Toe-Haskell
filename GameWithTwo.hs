module GameWithTwo (
    gameWithTwoPlayers,
    startGame,
    printSelection
)where

import Game

gameWithTwoPlayers :: IO ()
gameWithTwoPlayers = do
    let game = makeGame
    startGame game

startGame :: Game -> IO ()
startGame game = do
    printCurrentStateOfGame game
    printSelection game
    
    putStr "i: "
    tempi <- getLine
    let i = read tempi::Int

    putStr "j: "
    tempj <- getLine
    let j = read tempj::Int

    let markedGame = if isFreeCell game i j then (if playerMove game == 1 then rewriteGame (markX (table game) 0 i j) 0 else rewriteGame (markO (table game) 0 i j) 1) else rewriteGame (markO (table game) 0 (-1) (-1)) (playerMove game)
    
    let winner = returnWinner markedGame

    if winner == "" then startGame markedGame else if winner == "T" then putStrLn "Tie" else putStrLn ( "winner: " ++ winner )

printSelection :: Game -> IO ()
printSelection game = do
    let player = playerMove game
    putStr "Player "
    if player == 0 then putStr "O" else putStr "X"
    putStrLn " turn:"