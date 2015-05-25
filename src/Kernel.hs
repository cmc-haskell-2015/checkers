module Kernel ( PieceType(Man, King)
              , Color(White, Black)
              , Coord(Coord), crow, ccol
              , Piece(Piece), ptype, pcolor, ppos
              , CoordPair(CoordPair), cpfrom, cpto
              , Movement, mfrom, mto, meaten, mbecomeKing, mfirst
              , Infinitable(Finite, Infinity)
              , Direction
              , PieceConfig(PieceConfig), pcMoveDirs, pcEatDirs, pcMoveRadius,
                                          pcEatRadius, pcAfterEatRadius
              , GameInitStateGen
              , GameInitStateType(Regular, Inversed, Custom)
              , GameConfig(GameConfig), gcBoardSize, gcInitState, gcFirstColor,
                                        gcGreedy, gcWinner, gcMenConfig,
                                        gcKingConfig
              , defaultMenConfig, defaultKingConfig, defaultConfig
              , GameState
              , Game(Game), gcfg, gstate
              , getWinner
              , getPiece
              , willRemovePiece
              , getPiecesByColor
              , getAllMovesByColor
              , getAllMovesByCoord
              , getMovesByCoord
              , getMovesByColor
              , findMove
              , validMove
              , execMovement
              , unexecMovement
              , finishTurn
              , makeMove
              , initState
              , createGame ) where

import Data.List(find)

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
                         , mfirst :: Bool } deriving (Show, Eq)

data WinnerType = Normal | Reversed deriving (Show, Eq)

data Infinitable a = Finite a | Infinity deriving (Eq, Show)

instance Ord a => Ord (Infinitable a) where
    compare Infinity Infinity = EQ
    compare Infinity _ = GT
    compare _ Infinity = LT
    compare (Finite x) (Finite y) = compare x y
type Direction = Coord
data PieceConfig = PieceConfig { pcMoveDirs :: [Direction]
                               , pcEatDirs :: [Direction]
                               , pcMoveRadius :: Infinitable Int
                               , pcEatRadius :: Infinitable Int
                               , pcAfterEatRadius :: Infinitable Int }

defaultMenConfig :: PieceConfig
defaultMenConfig =
    PieceConfig [(Coord 1 (-1)), (Coord 1 1)]
                [(Coord (-1) (-1)), (Coord (-1) 1), (Coord 1 (-1)), (Coord 1 1)]
                (Finite 1) (Finite 1) (Finite 1)

defaultKingConfig :: PieceConfig
defaultKingConfig =
    PieceConfig [(Coord (-1) (-1)), (Coord (-1) 1), (Coord 1 (-1)), (Coord 1 1)]
                [(Coord (-1) (-1)), (Coord (-1) 1), (Coord 1 (-1)), (Coord 1 1)]
                Infinity Infinity Infinity

--                   board size
type GameInitStateGen = Int -> Coord -> Maybe Color
data GameInitStateType = Regular Int | Inversed Int | Custom GameInitStateGen
data GameConfig = GameConfig { gcBoardSize :: Int
                             , gcInitState :: GameInitStateType
                             , gcFirstColor :: Color
                             , gcGreedy :: Bool
                             , gcWinner :: WinnerType
                             , gcMenConfig :: PieceConfig
                             , gcKingConfig :: PieceConfig
                             , gcDeferRemoves :: Bool
                             , gcDeferBecomeKing :: Bool }

testInitState :: GameInitStateGen
testInitState _ c = if c == (Coord 2 1) || c == (Coord 1 4) || c == (Coord 5 2)
                    then Just White
                    else if c == (Coord 2 5) || c == (Coord 4 5)
                    then Just Black
                    else Nothing

defaultConfig :: GameConfig
defaultConfig = GameConfig 8 (Regular 3) White True Normal
                           defaultMenConfig defaultKingConfig True False

data GameState = GameState { gsField :: [Piece]
                           , gsRemove :: [Piece]
                           , gsUpdPiece :: Maybe Piece }

data Game = Game { gcfg :: GameConfig
                 , gstate :: GameState }

boardSize :: Int
boardSize = 8

piecesCount :: GameState -> Color -> Int
piecesCount (GameState field _ _) cl =
    length $ filter (\x -> (pcolor x) == cl) field

getWinner :: Game -> Maybe Color
getWinner (Game cfg state) =
    if (bcount > 0) == (wcount > 0)
    then Nothing
    else if (bcount > 0 && (gcWinner cfg) == Normal) ||
            (wcount > 0 && (gcWinner cfg) == Reversed)
    then Just Black
    else Just White
  where bcount = piecesCount state Black
        wcount = piecesCount state White

getPiece :: Game -> Coord -> Maybe Piece
getPiece (Game _ state) coord = find (\x -> (ppos x) == coord) (gsField state)

willRemovePiece :: Game -> Coord -> Bool
willRemovePiece (Game _ (GameState _ rm _)) coord =
    (find (\x -> (ppos x) == coord) rm) /= Nothing

getPiecesByColor :: Game -> Color -> [Piece]
getPiecesByColor (Game _ state) cl = filter (\x -> (pcolor x) == cl) (gsField state)

lastrow :: GameConfig -> Color -> Int
lastrow _ Black = 0
lastrow cfg _ = (gcBoardSize cfg) - 1

getPos :: Coord -> Direction -> Int -> Coord
getPos (Coord row col) (Coord drow dcol) len = Coord (row + len*drow) (col + len*dcol)

inField :: GameConfig -> Coord -> Bool
inField cfg (Coord row col) = row >= 0 && row < (gcBoardSize cfg) &&
                              col >= 0 && col < (gcBoardSize cfg)

isLastrow :: GameConfig -> Color -> Coord -> Bool
isLastrow cfg color (Coord row col) = (lastrow cfg color) == row

getSimpleMoves :: Game -> PieceConfig -> Piece -> Direction -> Int -> [Movement]
getSimpleMoves game@(Game cfg _) pconf piece@(Piece tp color from) dir len =
    if inField cfg cpos && (Finite len) <= (pcMoveRadius pconf) &&
       getPiece game cpos == Nothing
    then (Movement from cpos [] becomeKing True) :
         getSimpleMoves game pconf piece dir (len + 1)
    else []
  where
    cpos = (getPos from dir len)
    becomeKing = pieceTpChange tp (isLastrow cfg color cpos)

getEatMoves :: Game -> PieceConfig -> Piece -> Bool -> Direction ->
               Int -> Int -> Maybe Piece -> [Movement]
getEatMoves game@(Game cfg _) pconf piece@(Piece _ cl from) first dir len1 len2 Nothing =
    if inField cfg cpos && (Finite len1) <= (pcEatRadius pconf)
    then case getPiece game cpos of
           Nothing -> getEatMoves game pconf piece first dir (len1 + 1) 0 Nothing
           (Just p) -> if pcolor p /= cl && (not $ willRemovePiece game cpos)
                       then getEatMoves game pconf piece first dir len1 1 (Just p)
                       else []
    else []
  where
    cpos = (getPos from dir (len1 + len2))
getEatMoves game@(Game cfg _) pconf piece@(Piece tp cl from) first dir len1 len2 (Just eaten) =
    if inField cfg cpos && (Finite len2) <= (pcAfterEatRadius pconf) &&
       getPiece game cpos == Nothing
    then  (Movement from cpos [eaten] becomeKing first) :
          getEatMoves game pconf piece first dir len1 (len2 + 1) (Just eaten)
    else []
  where
    cpos = (getPos from dir (len1 + len2))
    becomeKing = pieceTpChange tp (isLastrow cfg cl cpos)

updDir :: Color -> Direction -> Direction
updDir Black (Coord drow dcol) = Coord (-drow) dcol
updDir _ d = d

getPieceMoves :: Game -> Piece -> Bool -> [Movement]
getPieceMoves game@(Game cfg _) piece@(Piece tp color _) first =
    simpleMoves ++ eatMoves
  where
    pconf = case tp of
              Man -> (gcMenConfig cfg)
              King -> (gcKingConfig cfg)
    simpleMoves =
        if first
        then concat $ [getSimpleMoves game pconf piece (updDir color dir) 1
                          | dir <- pcMoveDirs pconf]
        else []
    eatMoves = concat $ [getEatMoves game pconf piece first (updDir color dir) 1 0 Nothing
                          | dir <- pcEatDirs pconf]

getAllMovesByCoord :: Game -> Coord -> Bool -> [Movement]
getAllMovesByCoord g c first =
    case piece of
      Nothing -> []
      (Just p) -> getPieceMoves g p first
  where piece = (getPiece g c)

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

removePieces :: GameState -> Bool -> [Piece] -> GameState
removePieces state@(GameState field _ _) True rm =
    state { gsRemove = (gsRemove state) ++ rm }
removePieces state@(GameState field _ _) False rm =
    state { gsField = filter (cont rm) field }
  where
    cont :: [Piece] -> Piece -> Bool
    cont [] p = True
    cont (first:rest) p = first /= p && cont rest p

removePieceByCoord :: GameState -> Coord -> GameState
removePieceByCoord state@(GameState field _ _) c =
    state { gsField = filter (\x -> (ppos x) /= c) field }

addPiece :: GameState -> Piece -> GameState
addPiece state@(GameState field _ _) p = state { gsField = p : field }

updatePiece :: Movement -> Bool -> Piece -> Piece
updatePiece move@(Movement _ to _ bk _) False p =
    p { ptype = (pieceTpUpd (ptype p) bk)
      , ppos = to }
updatePiece move@(Movement _ to _ bk _) True p =
    p { ppos = to }

execMovementImpl :: Game -> Movement -> Piece -> Game
execMovementImpl game@(Game cfg state) move@(Movement from to eaten bk _) piece@(Piece tp _ _) =
    game { gstate = state3 }
  where
    updatedPiece = (updatePiece move (gcDeferBecomeKing cfg) piece)
    fullUpdPiece = (updatePiece move False piece)

    state1 = removePieces state (gcDeferRemoves cfg) eaten
    state2 = removePieceByCoord state1 (ppos piece)
    state3 = state2 { gsField = updatedPiece : (gsField state2)
                    , gsUpdPiece = case gsUpdPiece state2 of
                                     (Just p) -> Just $ p { ppos = to }
                                     Nothing -> Just fullUpdPiece }

execMovement :: Game -> Movement -> Game
execMovement game@(Game cfg state) move@(Movement from _ eaten bk _) =
    case cpiece of
      Nothing -> game
      (Just p) -> execMovementImpl game move p
  where
    cpiece = getPiece game from

unexecMovement :: Game -> Movement -> Game
unexecMovement g@(Game _ state) _ = g -- TODO

finishTurn :: Game -> Game
finishTurn game@(Game cfg state@(GameState field rm prepl)) =
    Game cfg remState { gsRemove = []
                      , gsUpdPiece = Nothing }
  where
    replState = case prepl of
                  Nothing -> state
                  (Just p) -> addPiece (removePieceByCoord state (ppos p)) p
    remState = removePieces replState False (gsRemove replState)

makeMove :: Game -> CoordPair -> Bool -> Game
makeMove g@(Game cfg state) cp first =
    case (findMove g cp first) of
      Nothing -> g
      (Just move) -> execMovement g move

defaultPieceGen :: Int -> Int -> Int -> Coord -> Maybe Color
defaultPieceGen boardSize mod_ n (Coord row col) =
    if ((row + col) `mod` 2) /= mod_ ||
       (row >= n && row < (boardSize - n))
    then Nothing
    else if row < n
    then Just White
    else Just Black

makePiece :: GameConfig -> Coord -> Maybe Piece
makePiece cfg coord = case colorGen coord of
                        Nothing -> Nothing
                        (Just c) -> Just $ Piece Man c coord
  where
    boardSize = gcBoardSize cfg
    colorGen = case gcInitState cfg of
                 Regular n -> defaultPieceGen boardSize 0 n
                 Inversed n -> defaultPieceGen boardSize 1 n
                 Custom f -> f boardSize

initState :: GameConfig -> GameState
initState cfg =
    GameState (toField [makePiece cfg (Coord row col) | row <- [0, 1 .. boardSize]
                                                      , col <- [0, 1 .. boardSize]])
              [] Nothing
  where
    boardSize = gcBoardSize cfg

    toField :: [Maybe Piece] -> [Piece]
    toField [] = []
    toField (Nothing:rest) = toField rest
    toField ((Just p):rest) = p:(toField rest)



createGame :: GameConfig -> Game
createGame cfg = Game cfg $ initState cfg
