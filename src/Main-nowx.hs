module Main where

import Kernel
import PlayerBase
import PlayerConsole
import DrawingConsole
import Controller

main :: IO ()
main = do
		--cp <- waitForMovement player1 game
		--putStrLn $ show $ cp
		winner <- run cfg player1 player2 drawing
		putStrLn $ (show winner) ++ " player wins!"
	where
		cfg = defaultConfig
		game = createGame cfg
		player1 = createPlayerConsole
		player2 = createPlayerConsole
		drawing = createDrawingConsole
