{-|
Module      : DrawingConsole
Description : Консольная система вывода состояния игры
License     : LGPLv3
-}
module DrawingConsole ( createDrawingConsole ) where

import Kernel
import DrawingBase( Drawing(Drawing) )

-- | Вернуть горизонтальную линию доски -- строку вида +---+---+
makeBorder :: Int -> String
makeBorder n = '+' : (concat $ replicate n "---+")

-- | Вернуть символ шашки по её типу и цвету
getPieceSym :: Color -> PieceType -> Char
getPieceSym White Man = 'o'
getPieceSym White King = '@'
getPieceSym Black Man = 'x'
getPieceSym Black King = '%'

-- | Символ-модификатор для шашек с отложенным удалением (съедением)
deferRmSym = '.'

-- | Символ-модификатор для шашек с отложеным переходом в дамки
deferBKSym = '*'

-- | Сгенерировать одну клетку игры (game -> row -> col)
makeCell :: Game -> Int -> Int -> String
makeCell game row col =
    [' ', cellSym, deferSym, '|']
  where
    makeCellImpl :: Maybe Piece -> Char
    makeCellImpl (Just p) = getPieceSym (pcolor p) (ptype p)
    makeCellImpl Nothing = ' '

    coord = (Coord row col)
    cellSym = makeCellImpl $ getPiece game coord
    deferSym = if willRemovePiece game coord
               then deferRmSym
               else ' '

-- | Сгенерировать строку доски
makeRow :: Game -> Int -> String
makeRow game@(Game cfg _) n =
    "   " ++ makeBorder boardSize ++ "\n" ++
    " " ++ (show (n + 1)) ++ " |" ++ cells ++ "\n"
  where
    boardSize = gcBoardSize cfg
    cells = (concat [makeCell game n k | k <- [0, 1 .. boardSize - 1]])

-- | Вернуть нижнюю строку с буквами столбцов
makeColLabels :: Int -> String
makeColLabels n = concat ["  " ++ [toLabel k] ++ " " | k <- [0, 1 .. n-1]]
  where
    toLabel :: Int -> Char
    toLabel k = toEnum (k + fromEnum 'A')


-- | Вернуть строку с отрисованным полем
makeField :: Game -> String
makeField game@(Game cfg _) =
    rows ++ "   " ++ (makeBorder boardSize) ++ "\n" ++
    "   " ++ (makeColLabels boardSize)
  where
    boardSize = gcBoardSize cfg
    rows = concat [makeRow game n | n <- [boardSize-1, boardSize-2 .. 0]]

-- | Отрисовать игру в консоль
repaint :: Game -> IO ()
repaint game@(Game cfg _) = do
    putStrLn $ makeField game
  where
    boardSize = gcBoardSize cfg

-- | Создать консольную систему вывода
createDrawingConsole :: Drawing
createDrawingConsole = Drawing repaint
