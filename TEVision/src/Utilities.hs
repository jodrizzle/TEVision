module Utilities (  
    getUprightBoundRect 
   ,getContours
   ,inContour 
   ,transparent
   ,white
   ,black
   ,blue
   ,green
   ,red
)   where

import Control.Monad (void)
import Control.Monad.Primitive
import Data.Proxy
import GHC.Int (Int32)
import GHC.Word 
import Filters
import Linear
import System.Environment 
import qualified OpenCV as CV
import Foreign.C.Types 
import qualified OpenCV.Core.Types.Point as P
import OpenCV.Internal.C.Types
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified OpenCV.ImgProc.StructuralAnalysis as SA 
import qualified Data.Vector as V

transparent, white, black, blue, green, red   :: CV.Scalar
transparent = CV.toScalar (V4 255 255 255   0 :: V4 Double)
white       = CV.toScalar (V4 255 255 255 255 :: V4 Double)
black       = CV.toScalar (V4   0   0   0 255 :: V4 Double)
blue        = CV.toScalar (V4 255   0   0 255 :: V4 Double)
green       = CV.toScalar (V4   0 255   0 255 :: V4 Double)
red         = CV.toScalar (V4   0   0 255 255 :: V4 Double)


-- minAreaRect : Finds a rotated rectangle of the minimum area enclosing the input 2D point set.
findEnclosingRectangle:: P.IsPoint2 point2 Int32 => V.Vector (point2 Int32) -> CV.RotatedRect
findEnclosingRectangle pts = SA.minAreaRect pts
    
getUprightBoundRect::   (V.Vector SA.Contour)-> CV.Rect2i 
getUprightBoundRect contours= CV.rotatedRectBoundingRect $ (findEnclosingRectangle (SA.contourPoints $ contours V.! 0))  --rect2i  

getContours :: PrimMonad m => M.Mat ('CV.S '[h0, w0]) ('CV.S 1) ('CV.S Word8) -> m (V.Vector SA.Contour)
getContours image = do
                    imageM <- CV.thaw image
                    contours_vector <- CV.findContours SA.ContourRetrievalExternal SA.ContourApproximationTC89KCOS imageM
                    pure contours_vector
                    
inContour:: (CV.IsPoint2 contourPoint2 CFloat, CV.IsPoint2 testPoint2 CFloat)=> V.Vector (contourPoint2 CFloat)->testPoint2 CFloat -> Bool
inContour cont pt
    | (CV.exceptError $ SA.pointPolygonTest cont pt False) >=0 = True
    | otherwise = False