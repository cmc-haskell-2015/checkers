module Main( main ) where

import Control.Concurrent

import qualified Graphics.UI.WX as Wx
import qualified Graphics.UI.WXCore as Wx
import qualified Graphics.UI.WXCore.Layout
import qualified Graphics.UI.WXCore.WxcTypes

import Kernel
import PlayerBase
import PlayerConsole
import DrawingBase
import DrawingConsole
import DrawingWx
import Controller

main
  = Wx.start gui

runGame :: Wx.Frame a -> GameConfig -> Player -> Player -> [Drawing] -> IO ()
runGame win cfg player1 player2 drawings = do
    winner <- run cfg player1 player2 drawings
    putStrLn $ (show winner) ++ " player wins!"
    return ()

gui :: IO ()
gui
  = do
    f <- Wx.frame [ Wx.text Wx.:= "wx test" ]

    drawingWx <- createDrawingWx f defaultConfig
    forkOS (runGame f cfg player1 player2 [drawingConsole, drawingWx])

    return ()
  where
    onClick :: Wx.Frame a -> IO ()
    onClick f
      = do
        Wx.close f
        return ()

    cfg = defaultConfig
    player1 = createPlayerConsole
    player2 = createPlayerConsole
    drawingConsole = createDrawingConsole