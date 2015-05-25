module Controller( run ) where

import Kernel
import PlayerBase ( Player
                  , waitForMovement
                  , badMovement
                  , invitePlayer )
import DrawingBase ( Drawing
                   , repaint )

nextColor :: Color -> Color
nextColor White = Black
nextColor _ = White

data GameEnv = GameEnv { geBlackPlayer :: Player
                       , geWhitePlayer :: Player
                       , geDrawing :: Drawing }

getPlayer :: GameEnv -> Color -> Player
getPlayer env cl = case cl of
                     Black -> (geBlackPlayer env)
                     White -> (geWhitePlayer env)

invalidMove :: GameEnv -> Game -> Color  -> IO Color
invalidMove env@(GameEnv _ _ drawing) game color = do
    badMovement (getPlayer env color)
    makeTurnImpl env game color Nothing

makeTurn :: GameEnv -> Game -> Color -> IO Color
makeTurn env@(GameEnv _ _ drawing) game color =
    case getWinner game of
      Nothing -> do
        invitePlayer (getPlayer env color) color
        makeTurnImpl env game color Nothing
      (Just winner) -> return winner

nextTurn :: GameEnv -> Game -> Color -> IO Color
nextTurn env game color = makeTurn env (finishTurn game) (nextColor color)

checkMovements :: Game -> Maybe Coord -> Bool
checkMovements game Nothing = True
checkMovements game (Just c) = (length $ getMovesByCoord game c False) > 0

processMoves :: GameEnv -> Game -> Color -> Maybe Coord -> [CoordPair] -> IO Color
processMoves env game color lastc [] = makeTurnImpl env game color lastc
processMoves env game color lastc (first:rest) =
    case move of
      Nothing -> invalidMove env game color
      (Just m) -> runNext (execMovement game m) m
  where
    move = findMove game first (lastc == Nothing)
    runNext :: Game -> Movement -> IO Color
    runNext game_ m = if (length $ meaten m) == 0
                      then nextTurn env game_ color
                      else processMoves env game_ color (Just $ mto m) rest

processMoves2 :: GameEnv -> Game -> Color -> Maybe Coord -> IO Color
processMoves2 env game color lastc = do
    moves <- waitForMovement (getPlayer env color) game color lastc
    processMoves env game color lastc moves

makeTurnImpl :: GameEnv -> Game -> Color -> Maybe Coord -> IO Color
makeTurnImpl env@(GameEnv _ _ drawing) game color lastc = do
    repaint drawing game
    if checkMovements game lastc
      then processMoves2 env game color lastc
      else nextTurn env game color

run :: GameConfig -> Player -> Player -> Drawing -> IO Color
run cfg bplayer wplayer drawing = do
    makeTurn (GameEnv bplayer wplayer drawing) game (gcFirstColor cfg)
  where
    game = createGame cfg
