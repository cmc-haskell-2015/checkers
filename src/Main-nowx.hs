module Main where

import Kernel
import PlayerConsole
import DrawingConsole
import Controller
import AIPlayer

main :: IO ()
main = do
    winner <- run cfg player1 player2 [drawing]
    case winner of
      Winner color -> putStrLn $ (show color) ++ " player wins!"
      DrawBy color -> putStrLn $ "It's a trap for " ++ (show color) ++ " player!"
    return ()
  where
    cfg = defaultConfig
    game = createGame cfg
    player1 = createAIPlayer 1 2
    player2 = createPlayerConsole 
    drawing = createDrawingConsole
