module Main where

import Kernel
import PlayerConsole
import DrawingConsole
import Controller
import AIPlayer
main :: IO ()
main = do
		winner <- run cfg player1 player2 [drawing]
		putStrLn $ (show winner) ++ " player wins!"
	where
		cfg = defaultConfig
		game = createGame cfg
		player1 = createPlayerConsole
		player2 = createPlayerConsole
		drawing = createDrawingConsole
