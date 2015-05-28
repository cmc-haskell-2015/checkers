{-|
Module      : AIPlayer
Description : Связующий модуль бота.
License     : LGPLv3

Модуль, отвечающий за рассчет хода бота для соответствующего состояния игры.
Реализованы 3 различных уровня сложности.
-}
module AIPlayer ( createAIPlayer,
                  AIAttr(AIAttr)) where

import AIPlayer.Internal