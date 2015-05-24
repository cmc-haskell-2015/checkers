module Kernel ( PieceType(Man, King)
              , Color(White, Black)
              , Coord(Coord), crow, ccol
              , Piece(Piece), ptype, pcolor, ppos
              , CoordPair(CoordPair), cpfrom, cpto
              , Movement, mfrom, mto, meaten, mbecomeKing, mfinal, mfirst
              , GameConfig(GameConfig), gcBoardSize, gcFirstColor, defaultConfig
              , GameState
              , Game(Game), gcfg, gstate
              , getWinner
              , getPiece
              , getPiecesByColor
              , getAllMovesByColor
              , getAllMovesByCoord
              , getMovesByCoord
              , getMovesByColor
              , findMove
              , validMove
              , execMovement
              , unexecMovement
              , makeMove
              , initState
              , createGame ) where

data PieceType = Man | King deriving (Show, Eq)

pieceTpUpd :: PieceType -> Bool -> PieceType
pieceTpUpd King _ = King
pieceTpUpd _ True = King
pieceTpUpd _ _ = Man

pieceTpChange :: PieceType -> Bool -> Bool
pieceTpChange King _ = False
pieceTpChange _ True = True
pieceTpChange _ _ = False

data Color = White | Black deriving (Show, Eq)

data Coord = Coord { crow :: Int
                   , ccol :: Int } deriving (Show, Eq)

data Piece = Piece { ptype :: PieceType
                   , pcolor :: Color
                   , ppos :: Coord } deriving (Show, Eq)

data CoordPair = CoordPair { cpfrom :: Coord
                           , cpto :: Coord } deriving (Show, Eq)
data Movement = Movement { mfrom :: Coord
                         , mto :: Coord
                         , meaten :: [Piece]
                         , mbecomeKing :: Bool
                         , mfinal :: Bool
                         , mfirst :: Bool } deriving (Show, Eq)

data GameConfig = GameConfig { gcBoardSize :: Int
                             , gcFirstColor :: Color
                             , isGreedy :: Bool}

defaultConfig :: GameConfig
defaultConfig = GameConfig 8 White True

type GameState = [Piece]
data Game = Game { gcfg :: GameConfig
                 , gstate :: GameState }

boardSize :: Int
boardSize = 8

piecesCount :: GameState -> Color -> Int
piecesCount state cl = length $ filter (\x -> (pcolor x) == cl) state

getWinner :: Game -> Maybe Color
getWinner (Game _ state) = let bcount = piecesCount state Black
                               wcount = piecesCount state White
                           in if bcount > 0 && wcount > 0
                              then Nothing
                              else if bcount == 0
                              then Just White
                              else Just Black

getPiece :: Game -> Coord -> Maybe Piece
getPiece (Game _ []) _ = Nothing
getPiece (Game cfg (p @ (Piece _ _ pc):rest)) c =
  if pc == c
  then Just p
  else getPiece (Game cfg rest) c

getPiecesByColor :: Game -> Color -> [Piece]
getPiecesByColor (Game _ state) cl = filter (\x -> (pcolor x) == cl) state


drow :: Color -> Int
drow Black = -1
drow _ = 1

lastrow :: GameConfig -> Color -> Int
lastrow _ Black = 0
lastrow cfg _ = (gcBoardSize cfg) - 1

getSimpleMovement :: Game -> Piece -> Coord -> Bool -> [Movement]
getSimpleMovement g@(Game cfg _) p@(Piece tp cl fc) rc@(Coord row col) first =
  if row >= 0 && row < (gcBoardSize cfg) &&
     col >= 0 && col < (gcBoardSize cfg) &&
     (getPiece g rc) == Nothing && first
  then [Movement fc rc [] (pieceTpChange tp (row == lastrow cfg cl)) True first]
  else []

eatThisPiece :: Piece -> Maybe Piece -> [Piece]
eatThisPiece (Piece _ cl1 _) (Just p@(Piece _ cl2 _)) = if cl1 == cl2
                                                        then []
                                                        else [p]
eatThisPiece _ Nothing = []

getComplexMovement :: Game -> Piece -> Coord -> Coord -> Bool -> [Movement]
getComplexMovement g@(Game cfg _) p@(Piece tp cl fc) tc rc@(Coord row col) first =
  let tpiece = getPiece g tc
      eaten = (eatThisPiece p tpiece)
  in
    if row >= 0 && row < (gcBoardSize cfg) &&
       col >= 0 && col < (gcBoardSize cfg) &&
       tpiece /= Nothing &&
       (getPiece g rc) == Nothing &&
       length eaten > 0
    then
      [Movement fc rc eaten
          (pieceTpChange tp (row == lastrow cfg cl)) False first]
    else []


getManMoves :: Game -> Piece -> Bool -> [Movement]
getManMoves g p@(Piece _ cl pos@(Coord row col)) first =
  (getSimpleMovement g p (Coord (row + (drow cl)) (col - 1)) first) ++
  (getSimpleMovement g p (Coord (row + (drow cl)) (col + 1)) first) ++
  (getComplexMovement g p (Coord (row - 1) (col - 1))
                          (Coord (row - 2) (col - 2)) first) ++
  (getComplexMovement g p (Coord (row - 1) (col + 1))
                          (Coord (row - 2) (col + 2)) first) ++
  (getComplexMovement g p (Coord (row + 1) (col - 1))
                          (Coord (row + 2) (col - 2)) first) ++
  (getComplexMovement g p (Coord (row + 1) (col + 1))
                          (Coord (row + 2) (col + 2)) first)

goKing :: Game -> Piece -> Coord -> Int -> Int -> Bool -> [Movement]
goKing g@(Game cfg _) p@(Piece _ _ pos) curPos@(Coord row col) dx dy first =
  if row >= 0 && row < (gcBoardSize cfg) &&
     col >= 0 && col < (gcBoardSize cfg) &&
     (getPiece g curPos) == Nothing
  then
    if first == True then
      [Movement pos curPos [] False True first] ++ (goKing g p (Coord (row + dx) (col + dy)) dx dy first)
    else
      (goKing g p (Coord (row + dx) (col + dy)) dx dy first)
  else
    let tpiece = (getPiece g curPos)
        eaten = (eatThisPiece p tpiece)
    in
      if (row + dx) >= 0 && (row + dx) < (gcBoardSize cfg) &&
        (col + dy) >= 0 && (col + dy) < (gcBoardSize cfg) &&
        (getPiece g curPos) /= Nothing &&
        (getPiece g (Coord (row + dx) (col + dy))) == Nothing &&
        length eaten > 0
      then
        [Movement pos (Coord (row + dx) (col + dy)) eaten False False first]
      else
        []

getKingMoves :: Game -> Piece -> Bool -> [Movement]
getKingMoves g p@(Piece _ _ pos@(Coord row col)) first =
  (goKing g p (Coord (row + 1) (col + 1)) 1 1 first) ++
  (goKing g p (Coord (row + 1) (col - 1)) 1 (-1) first) ++
  (goKing g p (Coord (row - 1) (col + 1)) (-1) 1 first) ++
  (goKing g p (Coord (row - 1) (col - 1)) (-1) (-1) first)


getPieceMoves :: Game -> Maybe Piece -> Bool -> [Movement]
getPieceMoves g (Just p@(Piece tp _ _)) first = case tp of
                                                  Man -> getManMoves g p first
                                                  King -> getKingMoves g p first
getPieceMoves _ Nothing _ = []

getAllMovesByCoord :: Game -> Coord -> Bool -> [Movement]
getAllMovesByCoord g c first = getPieceMoves g (getPiece g c) first

getAllMovesByColor :: Game -> Color -> [Movement]
getAllMovesByColor g cl =
    concat [getAllMovesByCoord g (ppos piece) True | piece <- getPiecesByColor g cl]

haveEating :: [Movement] -> Bool
haveEating ms = any (\m -> (length $ meaten m) > 0) ms

filterEatingMoves :: [Movement] -> [Movement]
filterEatingMoves ms = filter (\m -> (length $ meaten m) > 0) ms

getMovesByColor :: Game -> Color -> [Movement]
getMovesByColor g cl = let moves = getAllMovesByColor g cl in
  if (haveEating moves) then
    filterEatingMoves moves
  else
    moves

getMovesByCoord :: Game -> Coord -> Bool -> [Movement]   
getMovesByCoord g c first =
  let moves = getAllMovesByCoord g c first
      Just p@(Piece _ cl _) = (getPiece g c)
      allMoves = getAllMovesByColor g cl
  in
    if (haveEating allMoves) then
      filterEatingMoves moves
    else
      moves
  
findMove :: Game -> CoordPair -> Bool -> Maybe Movement
findMove g (CoordPair from to) first =
  let move = filter (\x -> (mto x) == to) $ getMovesByCoord g from first
  in if (length move) == 1
     then Just $ head move
     else Nothing

validMove :: Game -> CoordPair -> Bool -> Bool
validMove g cp first = (findMove g cp first) /= Nothing

removePieces :: GameState -> [Coord] -> GameState
removePieces state [] = state
removePieces state coords = filter (cont coords) state
  where
    cont [] p = True
    cont (first:rest) p = first /= (ppos p) && cont rest p

updatePiece :: Movement -> Piece -> Piece
updatePiece move@(Movement _ to _ bk _ _) p =
    p { ptype = (pieceTpUpd (ptype p) bk)
      , ppos = to }

execMovement :: Game -> Movement -> Game
execMovement game@(Game cfg state) move@(Movement from _ eaten _ _ _) =
    case cpiece of
      Nothing -> game
      (Just p) -> Game cfg ((updatePiece move p) : tempState)
  where
    eatenc = map (\x -> (ppos x)) eaten
    tempState = removePieces state ([from] ++ eatenc)
    cpiece = getPiece game from

unexecMovement :: Game -> Movement -> Game
unexecMovement g@(Game _ state) _ = g -- TODO

makeMove :: Game -> CoordPair -> Bool -> Game
makeMove g@(Game cfg state) cp first =
    case (findMove g cp first) of
      Nothing -> g
      (Just move) -> execMovement g move

initState :: GameConfig -> GameState
initState cfg = [(Piece Man White c) | c <- whitecoords] ++
               [(Piece Man Black c) | c <- blackcoords]
  where
    whitecoords = [(Coord 0 col) | col <- [0, 2 .. 6]] ++
                  [(Coord 1 col) | col <- [1, 3 .. 7]] ++
                  [(Coord 2 col) | col <- [0, 2 .. 6]]
    blackcoords = [(Coord 5 col) | col <- [1, 3 .. 7]] ++
                  [(Coord 6 col) | col <- [0, 2 .. 6]] ++
                  [(Coord 7 col) | col <- [1, 3 .. 7]]

createGame :: GameConfig -> Game
createGame cfg = Game cfg $ initState cfg
