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
import WxSuite
import Controller

main
  = Wx.start gui

runGame :: Wx.Frame a -> GameConfig -> Player -> Player -> [Drawing] -> IO ()
runGame win cfg player1 player2 drawings = do
    winner <- run cfg player1 player2 drawings
    case winner of
      Winner color -> putStrLn $ (show color) ++ " player wins!"
      DrawBy color -> putStrLn $ "It's a trap for " ++ (show color) ++ " player!"
    return ()

gui :: IO ()
gui
  = do
    f <- Wx.frame [ Wx.text Wx.:= "Шашки офигевашки" ]

    (drawingWx, canvas) <- createWxDrawing f defaultConfig
    playerWx1 <- createWxPlayer canvas
    playerWx2 <- createWxPlayer canvas
    forkIO (runGame f cfg playerWx1 playerWx2 [drawingConsole, drawingWx])

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