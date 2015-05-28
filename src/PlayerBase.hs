{-|
Module      : PlayerBase
Description : Модуль с определением типа системы ввода (игрока)
License     : LGPLv3
-}
module PlayerBase ( Player(Player)
                  , waitForMovement
                  , invitePlayer
                  , badMovement ) where

import Kernel( Game
             , Color
             , Coord
             , CoordPair )

-- | Тип абстрактного игрока. Может быть как произвольным интерфейсом для
-- | взаимодействия с пользователем, так и ИИ, считывалкой ходов из файла и проч.
data Player = Player
    { waitForMovement :: Game -> Color -> Maybe Coord -> IO [CoordPair] -- Вернуть ход пользователя
    , invitePlayer :: Color -> IO ()                        -- Выдать пользователю приглашение
    , badMovement :: IO () }                                -- Выдать пользователю предупреждение о плохом ходе
