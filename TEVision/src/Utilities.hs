module Utilities (  
    getUprightBoundRect 
   ,getContours
   ,inContour 
   ,getFeatures
   ,getDiffVectorXY
   ,getXComp
   ,getYComp
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
    
getFeatures:: depth `CV.In` '[CV.S Word8, CV.S Float, CV.D]=> M.Mat (CV.S '[h, w]) (CV.S 1) depth->V.Vector SA.Contour->V.Vector (V2 Float)
getFeatures canniedImg contours = CV.goodFeaturesToTrack canniedImg {-(fromIntegral (4*(V.length contours)))-} 1000 0.95 100 Nothing Nothing $ CV.HarrisDetector 0.2 --50 is the min distance between two features

     
getDiffVectorXY::V.Vector CV.Point2i->CV.Point2i->(V2 Int32 -> Int32)->[Int32]
getDiffVectorXY pts headPt getComp
    | V.length pts == 0 =  []
    | V.length pts == 1 =  [(getComp $ P.fromPoint headPt) - (getComp $ P.fromPoint $ pts V.! 0)]
    | otherwise         = [(getComp $ P.fromPoint $ pts V.! 1) - (getComp $ P.fromPoint $ pts V.! 0)] ++ (getDiffVectorXY (V.tail pts) headPt getComp)     
    
getXComp:: V2 Int32->Int32
getXComp (V2 x _) = x
-- 
getYComp::V2 Int32->Int32
getYComp (V2 _ y) = y