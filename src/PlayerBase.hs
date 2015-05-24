module PlayerBase ( Player(Player)
                  , waitForMovement
                  , invitePlayer
                  , badMovement ) where

import Kernel( Game
             , Color
             , Coord
             , CoordPair )

data Player = Player { pWaitForMovement :: Game -> Color -> Maybe Coord -> IO [CoordPair]
                     , pInvitePlayer :: Color -> IO ()
                     , pBadMovement :: IO () }

waitForMovement :: Player -> Game -> Color -> Maybe Coord -> IO [CoordPair]
waitForMovement player game color c = (pWaitForMovement player) game color c

invitePlayer :: Player -> Color -> IO ()
invitePlayer player color = (pInvitePlayer player) color

badMovement :: Player -> IO ()
badMovement player = pBadMovement player
