module AIPlayer ( createAIPlayer,
                  AIAttr(AIAttr)) where

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

data AIAttr = AIAttr { eatenPiece :: Int
                     , heldKing :: Int} deriving (Show, Eq)

         
                     
data AIMovement =  AIMovement { attr :: AIAttr
                              , attrEnemy :: AIAttr
                              , move :: CoordPair} deriving (Show, Eq)     
                              
defaultAIAttrConfig :: AIAttr
defaultAIAttrConfig = AIAttr 0 0

defaultCoordPair :: CoordPair
defaultCoordPair = CoordPair (Coord 0 0) (Coord 0 0)

defaultAIMovementConfig :: AIMovement
defaultAIMovementConfig = AIMovement defaultAIAttrConfig defaultAIAttrConfig defaultCoordPair
                  
sumAttr :: AIMovement -> AIMovement -> AIMovement
sumAttr p1@(AIMovement (AIAttr a b) (AIAttr aE bE) cp1) p2@(AIMovement (AIAttr c d) (AIAttr cE dE)  _) =
  AIMovement (AIAttr (a + c) (b + d)) (AIAttr (aE + cE) (bE + dE)) cp1

sumAttrEnemy :: AIMovement -> AIMovement -> AIMovement
sumAttrEnemy p1@(AIMovement (AIAttr a b) (AIAttr aE bE) cp1) p2@(AIMovement (AIAttr c d) (AIAttr cE dE)  _) =
  AIMovement (AIAttr (a + cE) (b + dE)) (AIAttr (aE + c) (bE + d)) cp1  

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

simpleMe :: [AIMovement] -> AIMovement
simpleMe [] = defaultAIMovementConfig
simpleMe (cur:rest) = simpleMax (simpleMe rest) cur

simpleEnemy :: [AIMovement] -> AIMovement
simpleEnemy [] = defaultAIMovementConfig
simpleEnemy (cur:rest) = simpleMax (simpleEnemy rest) cur


getBestMovement :: Int -> Int -> Int -> Game -> [Movement] -> [AIMovement] -> AIMovement 
getBestMovement lvl _ (-1) g [] allMove = 
  case lvl of
    1 -> simpleEnemy allMove
getBestMovement lvl _ 1 g [] allMove = 
  case lvl of
    1 -> simpleMe allMove
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
    
    
  
                     
waitForMovement :: Int -> Int -> Game -> Color -> Maybe Coord -> IO [CoordPair]
waitForMovement lvl deep g cl Nothing = 
  return $ [pair] where
    p@(AIMovement c1 cE pair) = getBestMovement lvl deep 1 g (getMovesByColor g cl) []
waitForMovement lvl deep g cl (Just c) = 
  return $ [pair] where
    p@(AIMovement c1 cE pair) = getBestMovement lvl deep 1 g (getMovesByCoord g c False) []


badMovement :: IO ()
badMovement = return ()

invitePlayer :: Color -> IO ()
invitePlayer _ = return ()

createAIPlayer :: Int -> Int -> Player
createAIPlayer lvl deep = Player (waitForMovement lvl deep) invitePlayer badMovement