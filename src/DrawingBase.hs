module DrawingBase ( Drawing(Drawing)
                   , repaint ) where

import Kernel

data Drawing = Drawing { dRepaint :: Game -> IO () }

repaint :: Drawing -> Game -> IO ()
repaint dr g = (dRepaint dr) g
