{-|
Module      : Kernel
Description : Модуль с основными типами и функциями.
License     : LGPLv3

Базовый модуль - "Ядро" программы. Реализована бОльшая часть всех типов и функций, 
необходимые для поддержания текущего состояния игры.
-}
module Kernel ( PieceType(Man, King)
              , Color(White, Black)
              , Winner(Winner, DrawBy)
              , Coord(Coord), crow, ccol
              , Piece(Piece), ptype, pcolor, ppos
              , CoordPair(CoordPair), cpfrom, cpto
              , Movement(Movement), mfrom, mto, meaten, mbecomeKing, mfirst
              , Infinitable(Finite, Infinity)
              , Direction
              , PieceConfig(PieceConfig), pcMoveDirs, pcEatDirs, pcMoveRadius,
                                          pcEatRadius, pcAfterEatRadius
              , GameInitStateGen
              , GameInitStateType(Regular, Inversed, Custom)
              , GameConfig(GameConfig), gcBoardSize, gcInitState, gcFirstColor,
                                        gcGreedy, gcWinner, gcMenConfig,
                                        gcKingConfig, gcDeferRemoves,
                                        gcDeferBecomeKing, gcEnableSeries
              , russianConfig, internationalConfig, englishConfig,
                armenianConfig, brazilianConfig, kanadianConfig, reversedConfig
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

-- | Описание 2-х состояний шашки
data PieceType =
  Man -- ^ Простая шашка 
  | King -- ^ Дамка
  deriving (Show, Eq)
  
-- | Обновление типа шашки, если она стала дамкой 
pieceTpUpd :: PieceType -> Bool -> PieceType
pieceTpUpd King _ = King
pieceTpUpd _ True = King
pieceTpUpd _ _ = Man

-- | Проверка, стоит ли обновлять тип шашки в дамки
pieceTpChange :: PieceType -> Bool -> Bool
pieceTpChange King _ = False
pieceTpChange _ True = True
pieceTpChange _ _ = False

-- | Тип цвета шашек
data Color =
  White -- ^ Белый цвет, по умолчанию делает первый ход
  | Black -- ^ Черный цвет, по умолчанию делает второй ход 
  deriving (Show, Eq)
  
-- | Тип для победителя, содержит цвет победителя, или цвет игрока, у которого патовая ситуация. 
data Winner =
  Winner Color -- ^ Соответственно цвет победителя
  | DrawBy Color -- ^ Цвет игрока с патовой ситуацией

-- | Тип координаты на доске
data Coord = Coord { crow :: Int -- ^ Координата строки (y-координата) 
                   , ccol :: Int -- ^ Координата столбца (x-координата)
                   }
                   deriving (Show, Eq)
                   
-- | Тип шашки
data Piece = Piece { ptype :: PieceType -- ^ Состояние шашки
                   , pcolor :: Color -- ^ Цвет шашки
                   , ppos :: Coord -- ^ Координата шашки
                   } deriving (Show, Eq)
                   
-- | Тип 2-х координат, нужен для определения координат хода шашки
data CoordPair = CoordPair { cpfrom :: Coord -- ^ Координата начала хода
                           , cpto :: Coord -- ^ Координата конца хода
                           } deriving (Show, Eq)

-- | Тип одиночного хода шашки
data Movement = Movement { mfrom :: Coord -- ^ Координата начала хода
                         , mto :: Coord -- ^ Координата конца хода
                         , meaten :: [Piece] -- ^ Список съеденных за ход шашек 
                         , mbecomeKing :: Bool -- ^ Стала ли шашка дамкой за данный ход
                         , mfirst :: Bool -- ^ Первый ли ход в серии ходов данной шашкой
                         } deriving (Show, Eq)

-- | Тип определения победителя, т.к. может быть случай поддавков
data WinnerType = 
  Normal -- ^ Стандартный тип победителя
  | Reversed -- ^ Поддавки
  deriving (Show, Eq)

-- | Специальный тип для определения конечного значения/бесконечности, нужен для ходов дамки  
data Infinitable a = 
  Finite a -- ^ Конечный тип
  | Infinity -- ^ Бесконечность
  deriving (Eq, Show)
  
-- | Перегрузка оператора сравнение бесконечного типа
instance Ord a => Ord (Infinitable a) where
    compare Infinity Infinity = EQ
    compare Infinity _ = GT
    compare _ Infinity = LT
    compare (Finite x) (Finite y) = compare x y
    
-- | Тип вектора направления    
type Direction = Coord

-- | Тип настроек шашек, объединенный для дамок и обычных шашек
data PieceConfig = PieceConfig { pcMoveDirs :: [Direction] -- ^ Список возможных направлений ходов
                               , pcEatDirs :: [Direction] -- ^ Список направлений, в которых мы съедим шашку противника
                               , pcMoveRadius :: Infinitable Int -- ^ Расстояние хода шашки 
                               , pcEatRadius :: Infinitable Int -- ^ Расстояния для съедения вражеской шашки
                               , pcAfterEatRadius :: Infinitable Int -- ^ Расстояние для возможного хода после съедения вражеской шашки
                               }

-- | Конфигурация для "русских" шашек, обычной шашки
russianMenConfig :: PieceConfig
russianMenConfig =
    PieceConfig [Coord 1 (-1), Coord 1 1]
                [Coord (-1) (-1), Coord (-1) 1, Coord 1 (-1), Coord 1 1]
                (Finite 1) (Finite 1) (Finite 1)

-- | Конфигурация для "английских" шашек, обычной шашки
englishMenConfig :: PieceConfig
englishMenConfig =
    PieceConfig [Coord 1 (-1), Coord 1 1]
                [Coord 1 (-1), Coord 1 1]
                (Finite 1) (Finite 1) (Finite 1)
                
-- | Конфигурация для "армянских" шашек, обычной шашки
armenianMenConfig :: PieceConfig
armenianMenConfig =
    PieceConfig [ Coord 0 (-1), Coord 1 (-1), Coord 1 0, Coord 1 1, Coord 0 1 ]
                [ Coord 0 (-1), Coord 1 0, Coord 0 1 ]
                (Finite 1) (Finite 1) (Finite 1)
                
-- | Конфигурация для "бразильских" шашек, обычной шашки
brazilianMenConfig :: PieceConfig
brazilianMenConfig =
    PieceConfig [Coord 1 (-1), Coord 1 1]
                [Coord 1 (-1), Coord 1 1]
                (Finite 1) (Finite 1) (Finite 1)

-- | Конфигурация для "русских" шашек, для дамки               
russianKingConfig :: PieceConfig
russianKingConfig =
    PieceConfig [ Coord (-1) (-1), Coord (-1) 1, Coord 1 (-1), Coord 1 1]
                [ Coord (-1) (-1), Coord (-1) 1, Coord 1 (-1), Coord 1 1]
                Infinity Infinity Infinity

-- | Конфигурация для "английских" шашек, для дамки                 
englishKingConfig :: PieceConfig
englishKingConfig =
    PieceConfig [ Coord (-1) (-1), Coord (-1) 1, Coord 1 (-1), Coord 1 1 ]
                [ Coord (-1) (-1), Coord (-1) 0, Coord (-1) 1
                , Coord 0 (-1), Coord 0 1
                , Coord 1 (-1), Coord 1 0, Coord 1 1 ]
                (Finite 1) (Finite 1) (Finite 1)

-- | Конфигурация для "американских" шашек, для дамки 
armenianKingConfig :: PieceConfig
armenianKingConfig =
    PieceConfig [ Coord (-1) (-1), Coord (-1) 0, Coord (-1) 1
                , Coord 0 (-1), Coord 0 1
                , Coord 1 (-1), Coord 1 0, Coord 1 1 ]
                [ Coord (-1) (-1), Coord (-1) 0, Coord (-1) 1
                , Coord 0 (-1), Coord 0 1
                , Coord 1 (-1), Coord 1 0, Coord 1 1 ]
                Infinity Infinity Infinity

-- |  Тип для генерации начального состояния игры, каждой шашки, нужен для тестовой доски
type GameInitStateGen =
  Int -- ^ Размер доски
  -> Coord -- ^ Коордианата шашки
  -> Maybe Color -- ^ Возможный цвет шашки
  
-- | Тип для различных базовых состояний доски
data GameInitStateType =
  Regular Int -- ^ Нормальный тип, размер доски
  | Inversed Int -- ^ Инверсированный тип, размер доски
  | Custom GameInitStateGen -- ^ Частный случай доски, нужен для тестирования

-- | Тип базовых настроек игры  
data GameConfig = GameConfig { gcBoardSize :: Int -- ^ Размер доски
                             , gcInitState :: GameInitStateType -- ^ Базовое состояние доски
                             , gcFirstColor :: Color -- ^ Цвет игрока, который делает первый ход
                             , gcGreedy :: Bool -- ^ Параметр, отвечающий за то, обязан ли есть вражескую шашку на своем ходу, если есть такая возможность
                             , gcWinner :: WinnerType -- ^ Параметр, отвечающий за тип выбора победителя
                             , gcMenConfig :: PieceConfig -- ^ Конфигурация обычной шашки
                             , gcKingConfig :: PieceConfig -- ^ Конфигурация дамки
                             , gcDeferRemoves :: Bool -- ^ Показывать ли промежуточные съедения шашек
                             , gcDeferBecomeKing :: Bool -- ^ Становится ли шашка дамкой сразу, в случае достижения границы доски в серии съедений
                             , gcEnableSeries :: Bool -- ^ Разрешено ли несколько ходов за серию 
                             }

-- | Первое тестовое начально состояние доски                            
testInitState :: GameInitStateGen
testInitState _ c = if c == (Coord 2 1) || c == (Coord 1 4) || c == (Coord 5 2)
                    then Just White
                    else if c == (Coord 2 5) || c == (Coord 4 5)
                    then Just Black
                    else Nothing

-- | Второе тестовое начальное состояние доски                    
testInitState2 :: GameInitStateGen
testInitState2 _ c = if c == (Coord 2 1)
                    then Just White
                    else if c == (Coord 3 2)
                    then Just Black
                    else Nothing

-- | Конфигурация для "русских" шашек, настройки игры
russianConfig :: GameConfig
russianConfig = GameConfig 8 (Regular 3) White True Normal
                           russianMenConfig russianKingConfig True False True

-- | Конфигурация для "международных" шашек, настройки игры                           
internationalConfig :: GameConfig
internationalConfig = GameConfig 10 (Regular 4) White True Normal
                                 russianMenConfig russianKingConfig True True True

-- | Конфигурация для "английских" шашек, настройки игры                                 
englishConfig :: GameConfig
englishConfig = GameConfig 8 (Regular 3) Black True Normal
                           englishMenConfig englishKingConfig True False True

-- | Конфигурация для "американских" шашек, настройки игры
armenianConfig :: GameConfig
armenianConfig = GameConfig 8 (Regular 3) White True Normal
                            armenianMenConfig armenianKingConfig False False True

-- | Конфигурация для "бразильских" шашек, настройки игры                            
brazilianConfig :: GameConfig
brazilianConfig = GameConfig 8 (Inversed 3) White True Normal
                             brazilianMenConfig russianKingConfig True True True

-- | Конфигурация для "канадских" шашек, настройки игры                            
kanadianConfig :: GameConfig
kanadianConfig = GameConfig 12 (Regular 5) White True Normal
                                 russianMenConfig russianKingConfig True True True

-- | Конфигурация для поддавков, настройки игры                                 
reversedConfig :: GameConfig
reversedConfig = GameConfig 8 (Regular 3) White True Reversed
                            russianMenConfig russianKingConfig True False True
                            
-- | Тип текущего состояния игры
data GameState = GameState { gsField :: [Piece] -- ^ Список всех шашек на доске
                           , gsRemove :: [Piece] -- ^ Список съеденных шашек
                           , gsUpdPiece :: Maybe Piece -- ^ Шашка, на которую нужно будет заменить ту шашку, которая ходила. 
                                                       -- ^ Необходимо для отложенного превращения в дамку
                           }
                           
-- | Главный тип игры
data Game = Game { gcfg :: GameConfig -- ^ Настройки игры
                 , gstate :: GameState -- ^ Текущее состояние игры
                 }
                 
-- | Размер доски по-умолчанию
boardSize :: Int
boardSize = 8

-- | Подсчет кол-ва шашек данного цвета
piecesCount :: GameState -> Color -> Int
piecesCount (GameState field _ _) cl =
    length $ filter (\x -> (pcolor x) == cl) field

-- | Проверка, является ли победителем игры игрок с заданным цветом
getWinner :: Game -> Color -> Maybe Winner
getWinner game@(Game cfg state) ccolor =
    if (bcount > 0) == (wcount > 0)
    then if length (getMovesByColor game ccolor) > 0
    then Nothing
    else Just $ DrawBy ccolor
    else if (bcount > 0 && (gcWinner cfg) == Normal) ||
            (wcount > 0 && (gcWinner cfg) == Reversed)
    then Just $ Winner Black
    else Just $ Winner White
  where bcount = piecesCount state Black
        wcount = piecesCount state White

-- | Получение шашки по координате        
getPiece :: Game -> Coord -> Maybe Piece
getPiece (Game _ state) coord = find (\x -> (ppos x) == coord) (gsField state)

-- | Проверка, удалена ли заданная шашка
willRemovePiece :: Game -> Coord -> Bool
willRemovePiece (Game _ (GameState _ rm _)) coord =
    (find (\x -> (ppos x) == coord) rm) /= Nothing

-- | Получение списка шашек заданного цвета    
getPiecesByColor :: Game -> Color -> [Piece]
getPiecesByColor (Game _ state) cl = filter (\x -> (pcolor x) == cl) (gsField state)

-- | Получение граничной строчки (на которой становится дамкой) для заданного цвета
lastrow :: GameConfig -> Color -> Int
lastrow _ Black = 0
lastrow cfg _ = (gcBoardSize cfg) - 1

-- | Получние коодинаты по начальной коодинате, направлению и расстоянию
getPos :: Coord -> Direction -> Int -> Coord
getPos (Coord row col) (Coord drow dcol) len = Coord (row + len*drow) (col + len*dcol)

-- | Проверка, находится ли коордианата в поле
inField :: GameConfig -> Coord -> Bool
inField cfg (Coord row col) = row >= 0 && row < (gcBoardSize cfg) &&
                              col >= 0 && col < (gcBoardSize cfg)

-- | Проверка, находимся ли мы на граничной строчке                              
isLastrow :: GameConfig -> Color -> Coord -> Bool
isLastrow cfg color (Coord row col) = (lastrow cfg color) == row


-- | Получение списка простых (без съедения) одиночных ходов шашки
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

-- | Получение списка одиночных ходов со съедением шашки    
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

    
-- | Обновление направления по цвету    
updDir :: Color -> Direction -> Direction
updDir Black (Coord drow dcol) = Coord (-drow) dcol
updDir _ d = d

-- | Получения возможных одиночных ходов заданной шашки
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

-- | Получение всех (и со съедением и без) одиночных ходов по координате                         
getAllMovesByCoord :: Game -> Coord -> Bool -> [Movement]
getAllMovesByCoord g c first =
    case piece of
      Nothing -> []
      (Just p) -> getPieceMoves g p first
  where piece = (getPiece g c)

-- | Получение всех (и со съедением и без) одиночных ходов по цвету    
getAllMovesByColor :: Game -> Color -> [Movement]
getAllMovesByColor g cl =
    concat [getAllMovesByCoord g (ppos piece) True | piece <- getPiecesByColor g cl]

-- | Проверка списка ходов на ход со съедением     
haveEating :: [Movement] -> Bool
haveEating ms = any (\m -> (length $ meaten m) > 0) ms

-- | Фильтрация ходов со съедением
filterEatingMoves :: [Movement] -> [Movement]
filterEatingMoves ms = filter (\m -> (length $ meaten m) > 0) ms

-- | Получение списка валидных ходов по цвету, т.е. удаление обычных ходов, если мы обязаны съесть
getMovesByColor :: Game -> Color -> [Movement]
getMovesByColor g cl = let moves = getAllMovesByColor g cl in
  if (haveEating moves) then
    filterEatingMoves moves
  else
    moves

-- | Получение списка валидных ходов по координате, т.е. удаление обычных ходов, если мы обязаны съесть    
getMovesByCoord :: Game -> Coord -> Bool -> [Movement]
getMovesByCoord g c first =
    case (getPiece g c) of
      Nothing -> []
      Just p -> getMovesByCoordImpl p
  where
    moves = getAllMovesByCoord g c first

    getMovesByCoordImpl :: Piece -> [Movement]
    getMovesByCoordImpl (Piece _ cl _) = if (haveEating allMoves)
                            then filterEatingMoves moves
                            else moves
      where
        allMoves = getAllMovesByColor g cl

-- | Поиск полного хода по частичной информации о ходе        
        
findMove :: Game -> CoordPair -> Bool -> Maybe Movement
findMove g (CoordPair from to) first =
  let move = filter (\x -> (mto x) == to) $ getMovesByCoord g from first
  in if (length move) == 1
     then Just $ head move
     else Nothing

-- | Проверка на корректность хода     
validMove :: Game -> CoordPair -> Bool -> Bool
validMove g cp first = (findMove g cp first) /= Nothing

-- | Удаление списка шашек с поля
removePieces :: GameState -> Bool -> [Piece] -> GameState
removePieces state@(GameState field _ _) True rm =
    state { gsRemove = (gsRemove state) ++ rm }
removePieces state@(GameState field _ _) False rm =
    state { gsField = filter (cont rm) field }
  where
    cont :: [Piece] -> Piece -> Bool
    cont [] p = True
    cont (first:rest) p = first /= p && cont rest p

-- | Удаление шашки с поля по координате    
removePieceByCoord :: GameState -> Coord -> GameState
removePieceByCoord state@(GameState field _ _) c =
    state { gsField = filter (\x -> (ppos x) /= c) field }

-- | Добавление новой шашки в состояние игры    
addPiece :: GameState -> Piece -> GameState
addPiece state@(GameState field _ _) p = state { gsField = p : field }

-- | Обновление состояние шашки, зная её ход
updatePiece :: Movement -> Bool -> Piece -> Piece
updatePiece move@(Movement _ to _ bk _) False p =
    p { ptype = (pieceTpUpd (ptype p) bk)
      , ppos = to }
updatePiece move@(Movement _ to _ bk _) True p =
    p { ppos = to }

-- | Реализация выполнения одиночного хода
    
execMovementImpl :: Game -> Movement -> Piece -> Game
execMovementImpl game@(Game cfg state) move@(Movement from to eaten bk _) piece@(Piece tp _ _) =
    game { gstate = state3 }
  where
    updatedPiece = (updatePiece move (gcDeferBecomeKing cfg) piece)

    state1 = removePieces state (gcDeferRemoves cfg) eaten
    state2 = removePieceByCoord state1 (ppos piece)
    state3 = state2 { gsField = updatedPiece : (gsField state2)
                    , gsUpdPiece = case gsUpdPiece state2 of
                                     (Just p) -> Just $ updatePiece move False p
                                     Nothing -> Just $ updatePiece move False piece }
-- | Выполнение одиночного хода, получаем итоговое состояние игры на выходе
execMovement :: Game -> Movement -> Game
execMovement game@(Game cfg state) move@(Movement from _ eaten bk _) =
    case cpiece of
      Nothing -> game
      (Just p) -> execMovementImpl game move p
  where
    cpiece = getPiece game from

-- | Отменить выполнение заданного одиночного хода    
unexecMovement :: Game -> Movement -> Game
unexecMovement g@(Game _ state) _ = g -- TODO

-- | Функция, отвечающая за обновление состояния игры, когда закончилась серия ходов
finishTurn :: Game -> Game
finishTurn game@(Game cfg state@(GameState field rm prepl)) =
    Game cfg remState { gsRemove = []
                      , gsUpdPiece = Nothing }
  where
    replState = case prepl of
                  Nothing -> state
                  (Just p) -> addPiece (removePieceByCoord state (ppos p)) p
    remState = removePieces replState False (gsRemove replState)

-- | Функция, которая вызывает выполнения одиночного хода    
makeMove :: Game -> CoordPair -> Bool -> Game
makeMove g@(Game cfg state) cp first =
    case (findMove g cp first) of
      Nothing -> g
      (Just move) -> execMovement g move

-- | Генератор состояни шашек по-умолчанию      
defaultPieceGen :: Int -> Int -> Int -> Coord -> Maybe Color
defaultPieceGen boardSize mod_ n (Coord row col) =
    if ((row + col) `mod` 2) /= mod_ ||
       (row >= n && row < (boardSize - n))
    then Nothing
    else if row < n
    then Just White
    else Just Black
    
-- | Функция, которая отвечает за определение шашки в состоянии игры
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
                 
-- | Инициализация игры в начальном состоянии
initState :: GameConfig -> GameState
initState cfg =
    GameState (toField [makePiece cfg (Coord row col) | row <- [0, 1 .. boardSize - 1]
                                                      , col <- [0, 1 .. boardSize - 1]])
              [] Nothing
  where
    boardSize = gcBoardSize cfg

    toField :: [Maybe Piece] -> [Piece]
    toField [] = []
    toField (Nothing:rest) = toField rest
    toField ((Just p):rest) = p:(toField rest)


-- | Главная функция создания игры
createGame :: GameConfig -> Game
createGame cfg = Game cfg $ initState cfg
