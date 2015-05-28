{-|
Module      : Kernel
Description : Связующий модуль модуль с основными типами и функциями.
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
              
import Kernel.Internal
              