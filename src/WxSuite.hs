module WxSuite( createWxDrawing
              , createWxPlayer ) where

import Control.Concurrent

import qualified Graphics.UI.WX as Wx
import qualified Graphics.UI.WXCore as Wx
import qualified Graphics.UI.WXCore.WxcTypes as Wx
import qualified Graphics.UI.WXCore.Draw as Wx

import Kernel
import DrawingBase( Drawing(Drawing) )
import PlayerBase( Player(Player) )

defaultCellSize = (Wx.sz 50 50) -- with border
borderSize = 1
borderColor = (Wx.rgb 0 0 0)

whiteColor = (Wx.rgb 230 220 210)
blackColor = (Wx.rgb 150 150 150)

data DrawingWxInfo = DrawingWxInfo { diCanvas :: Wx.Window () }

coord2point :: Int -> Int -> Int -> Int -> Wx.Point
coord2point w h x y = (Wx.point (x * w) (y * h))

loadPieceImg :: PieceType -> Color -> IO (Wx.Image ())
loadPieceImg Man White = Wx.imageCreateFromFile "resources/wman.png"
loadPieceImg Man Black = Wx.imageCreateFromFile "resources/niger.png"
loadPieceImg King White = Wx.imageCreateFromFile "resources/wking.png"
loadPieceImg King Black = Wx.imageCreateFromFile "resources/bking.png"

paintAtCoord :: Wx.DC() -> Wx.Rect -> Wx.Image () -> Coord -> Coord -> IO()
paintAtCoord dc rect@(Wx.Rect x y w h) img (Coord rows cols) (Coord row col) = do
    Wx.imageRescale img (Wx.rectSize rect)
    bitmap <- Wx.bitmapCreateFromImage img 32
    Wx.dcDrawBitmap dc bitmap (Wx.rectTopLeft rect) True
  where
    rect = Wx.rect (Wx.point (x + col * w `div` cols + borderSize)
                             (y + (rows - row - 1) * h `div` rows + borderSize))
                   (Wx.sz (w `div` cols - borderSize*2)
                          (h `div` rows - borderSize*2))

paintPiece :: Wx.DC() -> Wx.Rect -> Coord -> Piece -> IO()
paintPiece dc rect maxcoord (Piece tp cl coord) = do
    img <- loadPieceImg tp cl
    paintAtCoord dc rect img maxcoord coord

paintPiecesList :: Wx.DC() -> Wx.Rect -> Coord -> (Piece -> Bool) -> [Piece] -> IO()
paintPiecesList _ _ _ _ [] = return ()
paintPiecesList dc rect bsize tfunc (first:rest) = do
    paintPiece dc rect bsize first
    paintPiecesList dc rect bsize tfunc rest

paintPiecesEx :: Maybe Color -> Bool -> Game -> Wx.DC() -> Wx.Rect -> IO()
paintPiecesEx mcolor isFirst game@(Game cfg _) dc rect = do
    paintPiecesList dc rect bsize transp (getPiecesByColor game White)
    paintPiecesList dc rect bsize transp (getPiecesByColor game Black)
  where
    bsize = Coord (gcBoardSize cfg) (gcBoardSize cfg)

    transp :: Piece -> Bool
    transp (Piece _ pcolor pos) =
        case mcolor of
          Nothing -> True
          Just color -> (pcolor == color) && hasMoves
      where
        hasMoves = (length $ getMovesByCoord game pos isFirst) > 0

paintPieces :: Game -> Wx.DC() -> Wx.Rect -> IO ()
paintPieces game dc rect = paintPiecesEx Nothing True game dc rect

paintPossMovesImpl :: Wx.DC() -> Wx.Rect -> Coord -> [Coord] -> IO()
paintPossMovesImpl _ _ _ [] = return ()
paintPossMovesImpl dc rect bsize (first:rest) = do
    img <- Wx.imageCreateFromFile "resources/moving.png"
    paintAtCoord dc rect img bsize first
    paintPossMovesImpl dc rect bsize rest

paintPossibleMoves :: [Movement] -> Game -> Wx.DC() -> Wx.Rect -> IO()
paintPossibleMoves moves game@(Game cfg _) dc rect =
    paintPossMovesImpl dc rect bsize (map mto moves)
  where
    bsize = Coord (gcBoardSize cfg) (gcBoardSize cfg)

paintPossibleMovesByColor :: Color -> Game -> Wx.DC() -> Wx.Rect -> IO()
paintPossibleMovesByColor color game@(Game cfg _) dc rect =
    paintPossMovesImpl dc rect bsize (map mto moves)
  where
    bsize = Coord (gcBoardSize cfg) (gcBoardSize cfg)
    moves = (getMovesByColor game color)

paintPossibleMovesByCoord :: Coord -> Bool -> Game -> Wx.DC() -> Wx.Rect -> IO()
paintPossibleMovesByCoord coord isFirst game@(Game cfg _) dc rect =
    paintPossMovesImpl dc rect bsize (map mto moves)
  where
    bsize = Coord (gcBoardSize cfg) (gcBoardSize cfg)
    moves = (getMovesByCoord game coord isFirst)

paintBoard :: Game -> Wx.DC() -> Wx.Rect -> IO()
paintBoard (Game cfg _) dc rect =
    paintBoardRows dc rect boardSize boardSize boardSize
  where
    boardSize = (gcBoardSize cfg)

    paintBoardCols :: Wx.DC() -> Wx.Rect -> Int -> Int -> Int -> IO()
    paintBoardCols _ _ _ _ 0 = return ()
    paintBoardCols dc (Wx.Rect x y w h) cols row col = do
        (brush, bdel) <- Wx.brushCreateFromStyle $ Wx.brushSolid ccolor
        Wx.dcSetBrush dc brush
        Wx.dcDrawRectangle dc (Wx.rect (Wx.point x y) (Wx.sz cwidth h))
        paintBoardCols dc (Wx.rect (Wx.point (x + cwidth) y)
                          (Wx.sz (w - cwidth) h)) cols row (col - 1)
      where
        ccolor = if ((row + col) `mod` 2) == 0
                 then whiteColor
                 else blackColor
        cwidth = (w `div` col)

    paintBoardRows :: Wx.DC() -> Wx.Rect -> Int -> Int -> Int -> IO()
    paintBoardRows _ _ _ _ 0 = return ()
    paintBoardRows dc (Wx.Rect x y w h) rows cols row = do
        paintBoardCols dc (Wx.rect (Wx.point x y) (Wx.sz w (h `div` row))) cols row cols
        paintBoardRows dc (Wx.rect (Wx.point x (y + (h `div` row)))
                                   (Wx.sz w (h - (h `div` row)))) rows cols (row - 1)

complexOnPaint :: [Game -> Wx.DC() -> Wx.Rect -> IO()] -> Game -> Wx.DC() -> Wx.Rect -> IO()
complexOnPaint [] _ _ rect = return ()
complexOnPaint (first:rest) game dc rect = do
    first game dc rect
    complexOnPaint rest game dc rect

repaintGame :: DrawingWxInfo -> Game -> IO ()
repaintGame (DrawingWxInfo canvas) game = do
    Wx.windowOnPaint canvas (complexOnPaint [paintBoard, paintPieces] game)
    Wx.repaint canvas
    return ()

coord2color :: Int -> Int -> Wx.Color
coord2color row col = if ((row + col) `mod` 2) == 0
                      then whiteColor
                      else blackColor

initCanvasWindow :: Wx.Window a -> GameConfig -> IO (Wx.Window ())
initCanvasWindow parent cfg = do
    panel <- Wx.windowCreate parent Wx.idAny (Wx.rect Wx.pointNull totalSize) 0

    Wx.windowSetLayout parent (Wx.column 0 [Wx.alignCenter $ Wx.stretch $ Wx.widget panel])
    return $ Wx.downcastWindow panel
  where
    boardSize = (gcBoardSize cfg)
    totalSize = (Wx.sz ((Wx.sizeW defaultCellSize) * boardSize)
                       ((Wx.sizeH defaultCellSize) * boardSize))

badMovement :: IO ()
badMovement = putStrLn "Bad move, try again"

invitePlayer :: Color -> IO ()
invitePlayer color = putStrLn $ (show color) ++ " player, your turn!"

point2Coord :: Coord -> Wx.Size -> Wx.Point -> Coord
point2Coord (Coord rows cols) (Wx.Size width height) (Wx.Point px py) =
    Coord (rows - (py + cellHeight - 1) `div` cellHeight) (px `div` cellWidth)
  where
    cellHeight = (height `div` cols)
    cellWidth = (width `div` rows)

unbindMouse :: Wx.Window () -> IO ()
unbindMouse win = Wx.windowOnMouse win False emptyOnMouse
  where
    emptyOnMouse :: Wx.EventMouse -> IO()
    emptyOnMouse event = return ()

repaintWithState :: Wx.Window () -> Game -> Color -> Maybe Piece -> Bool -> IO ()
repaintWithState canvas game color mp isFirst = do
    Wx.windowOnPaint canvas (complexOnPaint paintActions game)
    Wx.repaint canvas
    return ()
  where
    moves = case mp of
              Nothing -> []
              Just (Piece _ _ pos) -> getMovesByCoord game pos isFirst
    paintActions = if length moves > 0
                   then [ paintBoard
                        , paintPossibleMoves moves
                        , paintPiecesEx (Just color) isFirst]
                   else [ paintBoard
                        , paintPiecesEx (Just color) isFirst]

onMouseEvent :: MVar Coord -> Wx.Size -> Coord -> Wx.EventMouse -> IO ()
onMouseEvent mvar size maxCoord event  =
    case event of
        (Wx.MouseLeftUp _ _) -> do
            putMVar mvar (point2Coord maxCoord size (Wx.mousePos event))
            Wx.skipCurrentEvent
        otherwise -> return ()


waitPieceSelect :: MVar Coord -> Wx.Window() -> Game -> Color -> IO CoordPair
waitPieceSelect mvar canvas game color = do
    repaintWithState canvas game color Nothing True
    coord <- takeMVar mvar
    case getPiece game coord of
      Nothing -> waitPieceSelect mvar canvas game color
      Just p@(Piece _ pcolor _) -> if pcolor == color
                                   then waitMoveSelect mvar canvas game p True
                                   else waitPieceSelect mvar canvas game color

waitMoveSelect :: MVar Coord -> Wx.Window() -> Game -> Piece -> Bool -> IO CoordPair
waitMoveSelect mvar canvas game piece@(Piece _ color pos) isFirst = do
    repaintWithState canvas game color (Just piece) isFirst
    coord <- takeMVar mvar

    case (elem coord moves, getPiece game coord, isFirst) of
      (True, _, _) -> return $ CoordPair pos coord
      (False, Nothing, True) -> waitPieceSelect mvar canvas game color
      (False, Just p, True) -> waitMoveSelect mvar canvas game p isFirst
      otherwise -> waitMoveSelect mvar canvas game piece isFirst

  where
    moves = map mto (getMovesByCoord game pos isFirst)


waitForMovement :: Wx.Window () -> Game -> Color -> Maybe Coord -> IO [CoordPair]
waitForMovement canvas game@(Game cfg _) color mc = do
    size <- Wx.windowGetSize canvas
    mvar <- newEmptyMVar
    Wx.windowOnMouse canvas False (onMouseEvent mvar size maxCoord)
    cpair <- case mp of
               Nothing -> waitPieceSelect mvar canvas game color
               Just p -> waitMoveSelect mvar canvas game p False
    unbindMouse canvas

    return [cpair]
  where
    maxCoord = Coord (gcBoardSize cfg) (gcBoardSize cfg)
    mp = case mc of
           Nothing -> Nothing
           Just c -> getPiece game c

createWxDrawing :: Wx.Window a -> GameConfig -> IO (Drawing, Wx.Window())
createWxDrawing parent cfg = do
    canvas <- initCanvasWindow parent cfg
    return (Drawing $ repaintGame (DrawingWxInfo canvas),
            Wx.downcastWindow canvas)

createWxPlayer :: Wx.Window () -> IO Player
createWxPlayer canvas =
    return $ Player (waitForMovement canvas) invitePlayer badMovement
