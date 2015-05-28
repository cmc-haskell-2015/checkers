{-|
Module      : PlayerConsole
Description : Консольная система ввода действий игроков
License     : LGPLv3
-}
module PlayerConsole ( createPlayerConsole ) where

import Kernel( Game(Game), getPiece
             , GameConfig
             , gcBoardSize
             , Color
             , Coord(Coord)
             , CoordPair(CoordPair)
             , Piece, pcolor
             , Movement, mfrom, mto
             , getMovesByCoord
             , getMovesByColor )
import PlayerBase ( Player(Player) )

-- | Конвертировать букву столбца в компоненту координаты
chr2col :: GameConfig -> Char -> Maybe Int
chr2col cfg ch = if cadiff >= 0 &&
                    cadiff < (gcBoardSize cfg)
                 then Just $ cadiff
                 else if cladiff >= 0 &&
                         cladiff < (gcBoardSize cfg)
                 then Just $ cladiff
                 else Nothing
  where
    ccode = fromEnum ch
    acode = fromEnum 'a'
    lacode = fromEnum 'A'
    cadiff = ccode - acode
    cladiff = ccode - lacode

-- | Конвертировать номер строки в компоненту координаты
chr2row :: GameConfig -> Char -> Maybe Int
chr2row cfg ch = if cndiff >= 0 &&
                    cndiff < (gcBoardSize cfg)
                 then Just $ cndiff
                 else Nothing
  where
    ccode = fromEnum ch
    ncode = fromEnum '1'
    cndiff = ccode - ncode

-- | Конвертировать цифро-буквенное представление координаты (напр., 'e2') в нормальное
str2coord :: GameConfig -> String -> Maybe Coord
str2coord cfg [x, y] = str2coordImpl (chr2row cfg y) (chr2col cfg x)
  where
    str2coordImpl :: Maybe Int ->  Maybe Int -> Maybe Coord
    str2coordImpl Nothing _ = Nothing
    str2coordImpl _ Nothing = Nothing
    str2coordImpl (Just row) (Just col) = Just $ Coord row col
str2coord _ _ = Nothing

-- | Перевести нормальную координату в цифро-буквенное представление
coord2str :: Coord -> String
coord2str (Coord row col) = [scol, srow]
  where
    scol = (toEnum $ col + (fromEnum 'a'))
    srow = (toEnum $ row + (fromEnum '1'))

-- | Разбить строку по данному разделителю
splitOn :: Char -> String -> [String]
splitOn ch s = splitOnImpl ch "" s
  where
    splitOnImpl :: Char -> String -> String -> [String]
    splitOnImpl _ "" "" = []
    splitOnImpl _ s "" = [s]
    splitOnImpl ch s (firsts:rests) = if firsts == ch
                                      then s:(splitOnImpl ch "" rests)
                                      else (splitOnImpl ch (s ++ [firsts]) rests)

-- | Убрать пробелы в начале и конце строки
strip :: String -> String
strip s = lstrip $ rstrip s
  where
    lstrip :: String -> String
    lstrip "" = ""
    lstrip (first:rest) = if first == ' '
                          then lstrip rest
                          else first:rest
    rstrip :: String -> String
    rstrip "" = ""
    rstrip s = if last s == ' '
              then rstrip $ init s
              else s

-- | Вывести список возможных ходов для данной клетки (если есть)
showMovesForCoord :: Game -> Color -> Coord -> Bool -> IO ()
showMovesForCoord game color c first =
    case piece of
      Nothing -> putStrLn $ "No piece at " ++ (coord2str c)
      (Just p) -> if pcolor p /= color
                  then putStrLn "Invalid piece color"
                  else putStrLn $ concat smoves
  where
    piece = getPiece game c
    moves = getMovesByCoord game c first
    smoves = [(coord2str c) ++ "-" ++ (coord2str (mto m)) ++ "\n" | m <- moves]

-- | Вывести список возможных ходов для данной клетки с учётом текущего состояния
-- | (первый-не первый ход в серии и т.д.)
showCoordMoves :: Game -> Color -> Maybe Coord -> Maybe Coord -> IO ()
showCoordMoves game color (Just c) fr = showMovesForCoord game color c (fr == Nothing)
showCoordMoves game color Nothing fr@(Just c) = showMovesForCoord game color c False
showCoordMoves _ _ Nothing Nothing = return ()

-- | Вывести список возможных ходов для игрока данного цвета
showColorMoves :: Game -> Color -> IO ()
showColorMoves game color = putStrLn $ concat smoves
  where
    moves = getMovesByColor game color
    smoves = [(coord2str (mfrom m)) ++ "-" ++ (coord2str (mto m)) ++ "\n" | m <- moves]

-- | Вывести сообщение и ждать нового ввода
badInput :: Game -> Color -> Maybe Coord -> String -> IO [CoordPair]
badInput game color c s = do
    putStrLn s
    waitForMovement game color c

-- | Разбить массив с серией ходов на пары (например, a1-c3-e5 на [a1-c3, c3-e5])
makeCoordPairs :: Game -> [Maybe Coord] -> [CoordPair]
makeCoordPairs _ [] = []
makeCoordPairs _ [x] = []
makeCoordPairs _ (Nothing:rest) = []
makeCoordPairs _ (_:Nothing:rest) = []
makeCoordPairs game lst =
    if (length lst) >= 2
    then ([CoordPair (extractMaybe (lst !! 0))
                     (extractMaybe (lst !! 1))] ++
        makeCoordPairs game (tail lst))
    else []
  where
    extractMaybe :: Maybe Coord -> Coord
    extractMaybe (Just x) = x

-- | Сравнить текущий цвет и цвет первой шашки в списке (если есть)
checkColor :: Game -> Color -> [Maybe Coord] -> Bool
checkColor _ _ [] = False
checkColor _ _ (Nothing:_) = False
checkColor game color ((Just coord):_) =
    case getPiece game coord of
      Nothing -> False
      Just p -> (pcolor p) == color

-- | Проверить, совпадает ли данная шашка (если есть) и первая шашка в списке
checkFirst :: Maybe Coord -> [Maybe Coord] -> Bool
checkFirst Nothing _ = True
checkFirst _ [] = False
checkFirst _ (Nothing:_) = False
checkFirst (Just c1) ((Just c2):_) = (c1 == c2)

-- | Обработать одну строку со вводом, вернуть список ходов
processLine :: Game -> Color -> Maybe Coord -> String -> IO [CoordPair]
processLine game@(Game cfg _) color c ('m':s) = do
    showCoordMoves game color (str2coord cfg (strip s)) c
    waitForMovement game color c
processLine game@(Game cfg _) color c "cm" = do
    showColorMoves game color
    waitForMovement game color c
processLine game@(Game cfg _) color c s =
    if length splitted < 2
    then badInput game color c "Too small input"
    else if any (\x -> x == Nothing) coords
    then badInput game color c "Bad input, try again"
    else if not $ checkFirst c coords
    then badInput game color c "Have to move a given piece"
    else if not $ checkColor game color coords
    then badInput game color c "Invalid piece color"
    else return $ makeCoordPairs game coords
  where
    splitted = map strip (splitOn '-' s)
    coords = map (str2coord cfg) splitted

-- | Ждать от пользователя ввода
waitForMovement :: Game -> Color -> Maybe Coord -> IO [CoordPair]
waitForMovement game color c = do
    line <- getLine
    processLine game color c line

-- | Высказать пользователю что-нибудь нехорошее о его последнем ходе
badMovement :: IO ()
badMovement = putStrLn "Bad move, try again"

-- | Напечатать приглашение ко вводу
invitePlayer :: Color -> IO ()
invitePlayer color = putStrLn $ (show color) ++ " player, your turn!"

-- | Создать консольный интерфейс для пользователя
createPlayerConsole :: Player
createPlayerConsole = Player waitForMovement invitePlayer badMovement
