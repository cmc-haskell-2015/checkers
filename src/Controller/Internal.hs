{-|
Module      : Controller.Internal
Description : Реализация модуля Controller
License     : LGPLv3
-}
module Controller.Internal where

import Kernel
import PlayerBase ( Player
                  , waitForMovement
                  , badMovement
                  , invitePlayer )
import DrawingBase ( Drawing
                   , repaint )

-- * Типы и вспомогательные функции

-- | Окружение игры.
data GameEnv = GameEnv
    { geBlackPlayer :: Player  -- ^ Чёрный игрок
    , geWhitePlayer :: Player  -- ^ Белый игрок
    , geDrawing :: [Drawing]   -- ^ Все системы вывода
    }

-- | Вызвать перерисовку у всех систем вывода
repaintAll :: Game -> [Drawing] -> IO ()
repaintAll _ [] = return ()
repaintAll game (first:rest) = do
    repaint first game
    repaintAll game rest

-- | Получить игрока по цвету
getPlayer :: GameEnv -> Color -> Player
getPlayer env cl = case cl of
                     Black -> (geBlackPlayer env)
                     White -> (geWhitePlayer env)

-- * Функции непосредственной работы контроллера

-- | Выдать сообщение о плохом ходе и запустить следующую итерацию ввода данных
invalidMove :: GameEnv -> Game -> Color -> IO Winner
invalidMove env game color = do
    badMovement (getPlayer env color)
    makeTurnImpl env game color Nothing

-- | Проверить, не закончилась ли партия и запустить итерацию хода
makeTurn :: GameEnv -> Game -> Color -> IO Winner
makeTurn env@(GameEnv _ _ drawings) game color =
    case getWinner game color of
      Nothing -> do
        invitePlayer (getPlayer env color) color
        makeTurnImpl env game color Nothing
      (Just winner) -> do
        repaintAll game drawings
        return winner

-- | Запустить следующую итерацию хода (закончив предыдущий ход и сменив цвет)
nextTurn :: GameEnv -> Game -> Color -> IO Winner
nextTurn env game color = makeTurn env (finishTurn game) (nextColor color)

-- | Проверить, может ли шашка ходить дальше
checkMovements :: Game -> Maybe Coord -> Bool
checkMovements game Nothing = True
checkMovements game (Just c) = (length $ getMovesByCoord game c False) > 0

-- | Проверить валидность списка пар координат, запустить Movement'ы, если они валидны
processMoves :: GameEnv -> Game -> Color -> Maybe Coord -> [CoordPair] -> IO Winner
processMoves env game color lastc [] = makeTurnImpl env game color lastc
processMoves env game@(Game cfg _) color lastc (first:rest) =
    case move of
      Nothing -> invalidMove env game color
      (Just m) -> runNext (execMovement game m) m
  where
    move = findMove game first (lastc == Nothing)
    runNext :: Game -> Movement -> IO Winner
    runNext game_ m = if (length $ meaten m) == 0 || (not $ gcEnableSeries cfg)
                      then nextTurn env game_ color
                      else processMoves env game_ color (Just $ mto m) rest

-- | Получить от пользователя ход и запустить его анализ
processMoves2 :: GameEnv -> Game -> Color -> Maybe Coord -> IO Winner
processMoves2 env game color lastc = do
    moves <- waitForMovement (getPlayer env color) game color lastc
    processMoves env game color lastc moves

-- | Перерисовать игру и передать управление игроку (или сменить игрока)
makeTurnImpl :: GameEnv -> Game -> Color -> Maybe Coord -> IO Winner
makeTurnImpl env@(GameEnv _ _ drawings) game color lastc = do
    repaintAll game drawings
    if checkMovements game lastc
      then processMoves2 env game color lastc
      else nextTurn env game color

-- | Главный цикл игры
run :: GameConfig -> Player -> Player -> [Drawing] -> IO Winner
run cfg bplayer wplayer drawings = do
    makeTurn (GameEnv bplayer wplayer drawings) game (gcFirstColor cfg)
  where
    game = createGame cfg
