{-|
Module      : AIPlayer.Internal
Description : Реализация модуля бота.
License     : LGPLv3

Модуль, отвечающий за рассчет хода бота для соответствующего состояния игры.
Реализованы 3 различных уровня сложности.
-}
module AIPlayer.Internal where

import Kernel( Game(Game), getPiece
             , GameConfig
             , Movement(Movement)
             , Piece(Piece)
             , gcBoardSize
             , Color
             , Coord(Coord)
             , CoordPair(CoordPair)
             , pcolor
             , getMovesByCoord
             , getMovesByColor
             , finishTurn
             , execMovement)
import PlayerBase ( Player(Player) )

-- * Типы и функции-утилиты с ними связанные

-- | Типа аттрибутов для соответствующего хода бота
data AIAttr = AIAttr { eatenPiece :: Double -- ^ Кол-во всех съеденных шашек в оптимальном случае, если бот сделает заданный ход
                     , heldKing :: Double -- ^ Кол-во проведенных дамок в оптимальном случае, если бот сделает заданный ход
                     } deriving (Show, Eq)

-- | Тип хода бота
data AIMovement =  AIMovement { attr :: AIAttr -- ^ Аттрибуты данного хода для бота
                              , attrEnemy :: AIAttr -- ^ Аттрибуты хода противника, если бот сделает заданный ход
                              , move :: CoordPair -- ^ Координаты хода бота
                              } deriving (Show, Eq)

-- | Параметры аттрибутов по умолчанию
defaultAIAttrConfig :: AIAttr
defaultAIAttrConfig = AIAttr (0) (0)

-- | Параметры координат по умолчанию
defaultCoordPair :: CoordPair
defaultCoordPair = CoordPair (Coord 0 0) (Coord 0 0)

-- | Параметры хода бота по умолчанию
defaultAIMovementConfig :: AIMovement
defaultAIMovementConfig = AIMovement defaultAIAttrConfig (AIAttr (100) (100)) defaultCoordPair

-- | Сложение 2-х ходов бота (складываются соответствующие аттрибуты), конечным ходом считается ход 1-го параметра
sumAttr :: AIMovement -> AIMovement -> AIMovement
sumAttr p1@(AIMovement (AIAttr a b) (AIAttr aE bE) cp1) p2@(AIMovement (AIAttr c d) (AIAttr cE dE)  _) =
  AIMovement (AIAttr (a + c) (b + d)) (AIAttr (aE + cE) (bE + dE)) cp1

-- | Сложение хода бота и следюущего хода противника, складываются противоположные аттрибуты, конечным ходом считается ход бота
sumAttrEnemy :: AIMovement -> AIMovement -> AIMovement
sumAttrEnemy p1@(AIMovement (AIAttr a b) (AIAttr aE bE) cp1) p2@(AIMovement (AIAttr c d) (AIAttr cE dE)  _) =
  AIMovement (AIAttr (a + cE) (b + dE)) (AIAttr (aE + c) (bE + d)) cp1

-- * Предопределённые оценочные и вспомогательные функции ИИ

-- | Оценочная функция для простого уровня сложности
simpleMax :: AIMovement -> AIMovement -> AIMovement
simpleMax p1@(AIMovement (AIAttr a b) (AIAttr aE bE) _) p2@(AIMovement (AIAttr c d) (AIAttr cE dE) _) =
  if (a > c) then
    p1
  else
    if (c > a) then
      p2
    else
      if (b > d) then
        p1
      else
        p2

-- | Вспомогательная Функция подсчета для среднего уровня сложности
aver :: AIAttr -> AIAttr -> Double
aver me@(AIAttr a b) enemy@(AIAttr c d) = (a + b * 8) / (a * b + 1) - ((c + d)) / (4 * c * d + 1)

-- | Оценочная функция для среднего уровня сложности
mediumMax :: AIMovement -> AIMovement -> AIMovement
mediumMax p1@(AIMovement me1@(AIAttr a b) enemy1@(AIAttr aE bE) _) p2@(AIMovement me2@(AIAttr c d) enemy2@(AIAttr cE dE) _) =
    if (aver me1 enemy1) > (aver me2 enemy2) then
      p1
    else
      p2

-- | Вспомогательная Функция подсчета для чуть лучшего среднего уровня сложности
logFrom :: AIAttr -> AIAttr -> Double
logFrom me@(AIAttr a b) enemy@(AIAttr c d) = 2 * (logBase 2 (2 * a * (4 * b + 1) + a + b + 1)) - (logBase 10 (((c * d)) / (c + d + 1) + 1)) - d * 2

-- | Оценочная функция для  чуть лучшего среднего уровня сложности
anotherMediumMax :: AIMovement -> AIMovement -> AIMovement
anotherMediumMax p1@(AIMovement me1@(AIAttr a b) enemy1@(AIAttr aE bE) _) p2@(AIMovement me2@(AIAttr c d) enemy2@(AIAttr cE dE) _) =
    if (logFrom me1 enemy1) > (logFrom me2 enemy2) then
      p1
    else
      p2

-- * Функции построения и анализа дерева ходов

-- | Функция рейтинга ходов бота. Выбирает оптимальный ход для заданного уровня сложности
ratingMe :: [AIMovement] -> Int -> AIMovement
ratingMe [] _ = defaultAIMovementConfig
ratingMe (cur:rest) lvl =
  case lvl of
    1 -> simpleMax (ratingMe rest lvl ) cur
    2 -> mediumMax (ratingMe rest lvl) cur
    3 -> anotherMediumMax (ratingMe rest lvl) cur

-- | Функция рейтинга ходов противника. Выбирает оптимальный ход для заданного уровня сложности
ratingEnemy :: [AIMovement] -> Int -> AIMovement
ratingEnemy [] _ = defaultAIMovementConfig
ratingEnemy (cur:rest) lvl =
  case lvl of
    1 -> simpleMax (ratingEnemy rest lvl) cur
    2 -> mediumMax (ratingEnemy rest lvl) cur
    3 -> anotherMediumMax (ratingEnemy rest lvl) cur

-- | Главная функция выбора наилучшего хода среди заданных возможных ходов, задается уровень сложности, глубина обхода.
-- | Рекурсивно находит для каждого "сына" дерева обхода оптимальный вариант хода, и исходя из оценочной функции выбирает свой ход
getBestMovement :: Int -> Int -> Int -> Game -> [Movement] -> [AIMovement] -> AIMovement
getBestMovement lvl _ (-1) g [] allMove = ratingEnemy allMove lvl
getBestMovement lvl _ 1 g [] allMove = ratingMe allMove lvl
getBestMovement lvl deep cl g (cur@(Movement fr to eat isKing first):rest) allMove =
  let
    curHeldKing = if isKing then 1 else 0
    addAIMovementHeldKing = AIMovement (AIAttr 0 curHeldKing) (AIAttr 0 0) (CoordPair fr to)
    addAIMovementHeldKingAndEaten = AIMovement (AIAttr 1 curHeldKing) (AIAttr 0 0) (CoordPair fr to)
    curDeep = if cl == 1 then deep else (deep - 1)
    Just p@(Piece _ color _) = (getPiece g fr)
    curAIMovement =
      if deep == 0 then
        if (length eat) > 0 then
          addAIMovementHeldKingAndEaten
        else
          addAIMovementHeldKing
      else
        if (length eat) == 0 then
          sumAttrEnemy addAIMovementHeldKing (getBestMovement lvl curDeep (cl * (-1)) newGame (getMovesByColor newGame color) [])
        else
          sumAttr addAIMovementHeldKingAndEaten (getBestMovement lvl deep cl newGame (getMovesByCoord newGame to False) []) where
            newGame =
              if ((length eat) == 0) then
                finishTurn (execMovement g cur)
              else
                execMovement g cur
  in
    getBestMovement lvl deep cl g rest (allMove ++ [curAIMovement]) where

-- * Функции из интерфейса Player

-- | Главная функция, которая выбирает какой должен сделать ход бот
waitForMovement :: Int -> Int -> Game -> Color -> Maybe Coord -> IO [CoordPair]
waitForMovement lvl deep g cl Nothing =
  return $ [pair] where
    p@(AIMovement c1 cE pair) = getBestMovement lvl deep 1 g (getMovesByColor g cl) []
waitForMovement lvl deep g cl (Just c) =
  return $ [pair] where
    p@(AIMovement c1 cE pair) = getBestMovement lvl deep 1 g (getMovesByCoord g c False) []

-- | Функция проверки на правильность хода
badMovement :: IO ()
badMovement = return ()

-- | "Приглашение" игрока сделать ход
invitePlayer :: Color -> IO ()
invitePlayer _ = return ()

-- | Основная функция создания бота, задается уровень сложности и глубина просмотра
createAIPlayer :: Int -> Int -> Player
createAIPlayer lvl deep = Player (waitForMovement lvl deep) invitePlayer badMovement