module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.UI.WXCore.Layout
import Graphics.UI.WXCore.WxcTypes

main
  = start gui

gui :: IO ()
gui
  = do
    f <- frame [ text := "wx test" ]

    img <- bitmapCreateFromFile "resources/welcome.jpg"
    bitmap <- staticBitmapCreate f 0 img rectNull 0

    btn <- button f [ text := "OK"
                    , on command := onClick f ]

    windowSetLayout f ( margin 10 $
                        column 5 [ alignCenter $ stretch $ widget bitmap
                                 , alignTopRight $ hstretch $ widget btn ])
    return ()
  where
    onClick :: Frame a -> IO ()
    onClick f
      = do
        close f
        return ()
