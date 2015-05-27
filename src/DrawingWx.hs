module DrawingWx( createDrawingWx ) where

import qualified Graphics.UI.WX as Wx
import qualified Graphics.UI.WXCore as Wx
import qualified Graphics.UI.WXCore.WxcTypes as Wx
import qualified Graphics.UI.WXCore.Draw as Wx

import Kernel
import DrawingBase( Drawing(Drawing) )

cellSize = (Wx.sz 50 50)
whiteColor = (Wx.rgb 230 220 210)
blackColor = (Wx.rgb 150 150 150)

data DrawWxInfo = DrawWxInfo { diCanvas :: Wx.Window () }

coord2point :: Int -> Int -> Int -> Int -> Wx.Point
coord2point w h x y = (Wx.point (x * w) (y * h))

loadPieceImg :: PieceType -> Color -> IO (Wx.Image ())
loadPieceImg Man White = Wx.imageCreateFromFile "resources/wman.png"
loadPieceImg Man Black = Wx.imageCreateFromFile "resources/niger.png"
loadPieceImg King White = Wx.imageCreateFromFile "resources/wking.png"
loadPieceImg King Black = Wx.imageCreateFromFile "resources/bking.png"

paintPiece :: Wx.DC() -> Wx.Rect -> Coord -> Piece -> IO()
paintPiece dc rect@(Wx.Rect x y w h) (Coord rows cols) (Piece tp cl (Coord row col)) = do
    img <- loadPieceImg tp cl
    Wx.imageRescale img (Wx.rectSize rect)
    bitmap <- Wx.bitmapCreateFromImage img 32
    Wx.dcDrawBitmap dc bitmap (Wx.rectTopLeft rect) True
  where
    rect = Wx.rect (Wx.point (x + col * w `div` cols) (y + (rows - row - 1) * h `div` rows))
                   (Wx.sz (w `div` cols) (h `div` rows))

paintPiecesList :: Wx.DC() -> Wx.Rect -> Coord -> [Piece] -> IO()
paintPiecesList _ _ _ [] = return ()
paintPiecesList dc rect bsize (first:rest) = do
    paintPiece dc rect bsize first
    paintPiecesList dc rect bsize rest

paintPieces :: Game -> Wx.DC() -> Wx.Rect -> IO()
paintPieces game@(Game cfg _) dc rect = do
    paintPiecesList dc rect bsize (getPiecesByColor game White)
    paintPiecesList dc rect bsize (getPiecesByColor game Black)
  where
    bsize = Coord (gcBoardSize cfg) (gcBoardSize cfg)

paintBoard :: Game -> Wx.DC() -> Wx.Rect -> IO()
paintBoard (Game cfg _) dc rect = paintBoardRows dc rect boardSize boardSize boardSize
  where
    boardSize = (gcBoardSize cfg)

    paintBoardCols :: Wx.DC() -> Wx.Rect -> Int -> Int -> Int -> IO()
    paintBoardCols _ _ _ _ 0 = return ()
    paintBoardCols dc (Wx.Rect x y w h) cols row col = do
        (brush, del) <- Wx.brushCreateFromStyle $ Wx.brushSolid ccolor
        Wx.dcSetBrush dc brush
        Wx.dcDrawRectangle dc (Wx.rect (Wx.point x y) (Wx.sz (w `div` col) h))
        paintBoardCols dc (Wx.rect (Wx.point (x + (w `div` col)) y)
                          (Wx.sz (w - (w `div` col)) h)) cols row (col - 1)
      where
        ccolor = if ((row + col) `mod` 2) == 0
                 then whiteColor
                 else blackColor

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

repaintGame :: DrawWxInfo -> Game -> IO ()
repaintGame (DrawWxInfo canvas) game = do
    Wx.windowOnPaint canvas (complexOnPaint [paintBoard, paintPieces] game)
    Wx.repaint canvas
    return ()


coord2color :: Int -> Int -> Wx.Color
coord2color row col = if ((row + col) `mod` 2) == 0
                      then whiteColor
                      else blackColor

initWindow :: Wx.Window a -> GameConfig -> IO DrawWxInfo
initWindow parent cfg = do
    panel <- Wx.windowCreate parent Wx.idAny (Wx.rect Wx.pointNull totalSize) 0

    Wx.windowSetLayout parent (Wx.column 0 [Wx.alignCenter $ Wx.stretch $ Wx.widget panel])
    return $ DrawWxInfo (Wx.downcastWindow panel)
  where
    boardSize = (gcBoardSize cfg)
    totalSize = (Wx.sz ((Wx.sizeW cellSize) * boardSize)
                ((Wx.sizeH cellSize) * boardSize))

createDrawingWx :: Wx.Window a -> GameConfig -> IO Drawing
createDrawingWx parent cfg = do
    dinfo <- initWindow parent cfg
    return $ Drawing $ repaintGame dinfo
