module Utilities (  
    findEnclosingRectangle
   ,getContours
   ,getPt 
   ,getUprightBoundRect
   ,getXComp
   ,getYComp
   ,makePoint2f
   ,orderPts
   ,transparent
   ,white
   ,black
   ,blue
   ,green
   ,red
)   where

import Control.Monad (void)
import Control.Monad.Primitive
import Data.Function
import Data.List
import Foreign.C.Types
import GHC.Int (Int32)
import GHC.Word 
import Linear
import OpenCV.Internal.C.Types
import System.Environment 
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified OpenCV.Core.Types.Point as P
import qualified OpenCV.ImgProc.StructuralAnalysis as SA 
import qualified Data.Vector as V

transparent, white, black, blue, green, red   :: CV.Scalar
transparent = CV.toScalar (V4 255 255 255   0 :: V4 Double)
white       = CV.toScalar (V4 255 255 255 255 :: V4 Double)
black       = CV.toScalar (V4   0   0   0 255 :: V4 Double)
blue        = CV.toScalar (V4 255   0   0 255 :: V4 Double)
green       = CV.toScalar (V4   0 255   0 255 :: V4 Double)
red         = CV.toScalar (V4   0   0 255 255 :: V4 Double)

orderPts::V.Vector CV.Point2i->V.Vector CV.Point2i
orderPts pts = V.fromList [topLeft pts, topRight pts, bottomLeft pts, bottomRight pts]

topLeft::V.Vector CV.Point2i->CV.Point2i
topLeft     = head . take 1 . sortBy (compare `on` (getXComp . P.fromPoint)) . take 2 . sortBy (compare `on` (getYComp . P.fromPoint)) . V.toList
topRight::V.Vector CV.Point2i->CV.Point2i
topRight    = head . drop 1 . sortBy (compare `on` (getXComp . P.fromPoint)) . take 2 . sortBy (compare `on` (getYComp . P.fromPoint)) . V.toList
bottomLeft::V.Vector CV.Point2i->CV.Point2i
bottomLeft  = head . take 1 . sortBy (compare `on` (getXComp . P.fromPoint)) . drop 2 . sortBy (compare `on` (getYComp . P.fromPoint)) . V.toList
bottomRight::V.Vector CV.Point2i->CV.Point2i
bottomRight = head . drop 1 . sortBy (compare `on` (getXComp . P.fromPoint)) . drop 2 . sortBy (compare `on` (getYComp . P.fromPoint)) . V.toList

getPt::Int->V.Vector P.Point2i->V2 Int32
getPt num a = P.fromPoint $ a V.! (num-1)

makePoint2f::V2 Int32->V2 CFloat
makePoint2f (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

getXComp::V2 Int32->Int32
getXComp (V2 x _) = x
getYComp::V2 Int32->Int32
getYComp (V2 _ y) = y

findEnclosingRectangle:: P.IsPoint2 point2 Int32 => V.Vector (point2 Int32) -> CV.RotatedRect
findEnclosingRectangle = SA.minAreaRect
    
getUprightBoundRect::   (V.Vector SA.Contour)-> CV.Rect2i 
getUprightBoundRect contours= CV.rotatedRectBoundingRect $ (findEnclosingRectangle (SA.contourPoints $ contours V.! 0))  --rect2i  

getContours :: PrimMonad m => M.Mat ('CV.S '[h0, w0]) ('CV.S 1) ('CV.S Word8) -> m (V.Vector SA.Contour)
getContours image = do
                    imageM <- CV.thaw image
                    contours_vector <- CV.findContours SA.ContourRetrievalExternal SA.ContourApproximationTC89KCOS imageM
                    pure contours_vector