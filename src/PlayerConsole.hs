module PlayerConsole ( createPlayerConsole ) where

import Kernel( Game(Game), getPiece
             , GameConfig
             , gcBoardSize
             , Color
             , Coord(Coord)
             , CoordPair(CoordPair)
             , Piece, pcolor
             , Movement, mfrom, mto
             , getMovesByCoord )
import PlayerBase ( Player(Player) )

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

chr2row :: GameConfig -> Char -> Maybe Int
chr2row cfg ch = if cndiff >= 0 &&
                    cndiff < (gcBoardSize cfg)
                 then Just $ cndiff
                 else Nothing
  where
    ccode = fromEnum ch
    ncode = fromEnum '1'
    cndiff = ccode - ncode

str2coord :: GameConfig -> String -> Maybe Coord
str2coord cfg [x, y] = str2coordImpl (chr2row cfg y) (chr2col cfg x)
  where
    str2coordImpl :: Maybe Int ->  Maybe Int -> Maybe Coord
    str2coordImpl Nothing _ = Nothing
    str2coordImpl _ Nothing = Nothing
    str2coordImpl (Just row) (Just col) = Just $ Coord row col
str2coord _ _ = Nothing

coord2str :: Coord -> String
coord2str (Coord row col) = [scol, srow]
  where
    scol = (toEnum $ col + (fromEnum 'a'))
    srow = (toEnum $ row + (fromEnum '1'))

splitOn :: Char -> String -> [String]
splitOn ch s = splitOnImpl ch "" s
  where
    splitOnImpl :: Char -> String -> String -> [String]
    splitOnImpl _ "" "" = []
    splitOnImpl _ s "" = [s]
    splitOnImpl ch s (firsts:rests) = if firsts == ch
                                      then s:(splitOnImpl ch "" rests)
                                      else (splitOnImpl ch (s ++ [firsts]) rests)

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

showMovesForCoord :: Game -> Color -> Coord -> Bool -> IO ()
showMovesForCoord game color c first =
    if piece == Nothing
    then putStrLn $ "No piece at " ++ (coord2str c)
    else if getColor piece /= color
    then putStrLn "Invalid piece color"
    else putStrLn $ concat $ smoves
  where
    piece = getPiece game c
    getColor :: Maybe Piece -> Color
    getColor (Just p) = (pcolor p)
    moves = getMovesByCoord game c first
    smoves = [(coord2str c) ++ "-" ++ (coord2str (mto m)) ++ "\n" | m <- moves]

showMoves :: Game -> Color -> Maybe Coord -> Maybe Coord -> IO ()
showMoves game color (Just c) fr = showMovesForCoord game color c (fr == Nothing)
showMoves game color Nothing fr@(Just c) = showMovesForCoord game color c False
showMoves _ _ Nothing Nothing = return ()

badInput :: Game -> Color -> Maybe Coord -> String -> IO [CoordPair]
badInput game color c s = do
    putStrLn s
    waitForMovement game color c

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

checkColor :: Game -> Color -> [Maybe Coord] -> Bool
checkColor _ _ [] = False
checkColor _ _ (Nothing:_) = False
checkColor game color ((Just coord):_) =
    if piece /= Nothing
    then (pcolor $ extractMaybe piece) == color
    else False
  where
    piece = getPiece game coord
    extractMaybe :: Maybe Piece -> Piece
    extractMaybe (Just x) = x

checkFirst :: Maybe Coord -> [Maybe Coord] -> Bool
checkFirst Nothing lst = True
checkFirst _ [] = False
checkFirst _ (Nothing:_) = False
checkFirst (Just c1) ((Just c2):_) = (c1 == c2)

processLine :: Game -> Color -> Maybe Coord -> String -> IO [CoordPair]
processLine game@(Game cfg _) color c ('m':s) = do
    showMoves game color c (str2coord cfg (strip s))
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

waitForMovement :: Game -> Color -> Maybe Coord -> IO [CoordPair]
waitForMovement game color c = do
    line <- getLine
    processLine game color c line

badMovement :: IO ()
badMovement = putStrLn "Bad move, try again"

invitePlayer :: Color -> IO ()
invitePlayer color = putStrLn $ (show color) ++ " player, your turn!"

createPlayerConsole :: Player
createPlayerConsole = Player waitForMovement invitePlayer badMovement
