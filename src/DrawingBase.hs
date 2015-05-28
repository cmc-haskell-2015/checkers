{-|
Module      : DrawingBase
Description : Модуль с определением типа системы вывода
License     : LGPLv3
-}
module DrawingBase ( Drawing(Drawing)
                   , repaint ) where

import Kernel

-- | Тип, описывающий систему вывода
data Drawing = Drawing
	{ repaint :: Game -> IO () -- ^ Перерисовать текущее состояние игры
	}
