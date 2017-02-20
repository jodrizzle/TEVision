module Utilities (  
    findEnclosingRectangle
   ,getContours
   ,getDiffList
   ,getDiffVectorXY
   ,getFeatures
   ,getPt 
   ,getRectCorners
   ,getUprightBoundRect
   ,getIntXComp
   ,getIntYComp
   ,getXComp
   ,getYComp
   ,inContour 
   ,makePoint2i
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
import Data.Proxy
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

import Filters
import ModuleXOR

type RectCornersFloat = (CV.Point2f,CV.Point2f,CV.Point2f,CV.Point2f)

transparent, white, black, blue, green, red   :: CV.Scalar
transparent = CV.toScalar (V4 255 255 255   0 :: V4 Double)
white       = CV.toScalar (V4 255 255 255 255 :: V4 Double)
black       = CV.toScalar (V4   0   0   0 255 :: V4 Double)
blue        = CV.toScalar (V4 255   0   0 255 :: V4 Double)
green       = CV.toScalar (V4   0 255   0 255 :: V4 Double)
red         = CV.toScalar (V4   0   0 255 255 :: V4 Double)

orderPts::RectCornersFloat-> RectCornersFloat --TL,TR,BL,BR
orderPts pts = (topLeft pts, topRight pts, bottomLeft pts, bottomRight pts) 

topLeft::RectCornersFloat->CV.Point2f
topLeft     = P.toPoint . listToValue . take 1 . sortBy (compare `on` getXComp) . take 2 . sortBy (compare `on` getYComp) . makeList

topRight::RectCornersFloat->CV.Point2f
topRight    = P.toPoint . listToValue . drop 1 . sortBy (compare `on` getXComp) . take 2 . sortBy (compare `on` getYComp) . makeList

bottomLeft::RectCornersFloat->CV.Point2f
bottomLeft  = P.toPoint . listToValue . take 1 . sortBy (compare `on` getXComp) . drop 2 . sortBy (compare `on` getYComp) . makeList

bottomRight::RectCornersFloat->CV.Point2f
bottomRight = P.toPoint . listToValue . drop 1 . sortBy (compare `on` getXComp) . drop 2 . sortBy (compare `on` getYComp) . makeList

listToValue::[V2 CFloat]->V2 CFloat
listToValue (x:xs) = x

makeList::RectCornersFloat->[V2 CFloat]
makeList (x,y,z,w) = [ P.fromPoint x, P.fromPoint y, P.fromPoint z, P.fromPoint w ]

getPt::Int32->RectCornersFloat->V2 Int32
getPt num (x,y,z,w)
    |   num==1 = makePoint2i $ P.fromPoint x
    |   num==2 = makePoint2i $ P.fromPoint y
    |   num==3 = makePoint2i $ P.fromPoint z
    |   num==4 = makePoint2i $ P.fromPoint w

makePoint2i::(V2 CFloat)->V2 Int32
makePoint2i (V2 x y) = V2 (round x) (round y)

getXComp::V2 CFloat->CFloat
getXComp (V2 x _) =  x 
getYComp::V2 CFloat->CFloat
getYComp (V2 _ y) =  y 

getIntXComp (V2 x _) = x
getIntYComp (V2 _ y) = y
-- minAreaRect : Finds a rotated rectangle of the minimum area enclosing the input 2D point set.
findEnclosingRectangle:: P.IsPoint2 point2 Int32 => V.Vector (point2 Int32) -> CV.RotatedRect
findEnclosingRectangle = SA.minAreaRect
    
getUprightBoundRect::   (V.Vector SA.Contour)-> CV.Rect2i 
getUprightBoundRect contours= CV.rotatedRectBoundingRect $ (findEnclosingRectangle (SA.contourPoints $ contours V.! 0))  --rect2i  

getRectCorners::CV.RotatedRect->RectCornersFloat
getRectCorners = CV.rotatedRectPoints

getContours :: PrimMonad m => M.Mat ('CV.S '[h0, w0]) ('CV.S 1) ('CV.S Word8) -> m (V.Vector SA.Contour)
getContours image = do
                    imageM <- CV.thaw image
                    contours_vector <- CV.findContours SA.ContourRetrievalExternal SA.ContourApproximationTC89KCOS imageM
                    pure contours_vector
                    
inContour:: (CV.IsPoint2 contourPoint2 CFloat, CV.IsPoint2 testPoint2 CFloat)=> V.Vector (contourPoint2 CFloat)->testPoint2 CFloat -> Bool
inContour cont pt
    | (CV.exceptError $ SA.pointPolygonTest cont pt False) >=0 = True
    | otherwise                                                = False
    
getFeatures:: depth `CV.In` '[CV.S Word8, CV.S Float, CV.D]=> M.Mat (CV.S '[h, w]) (CV.S 1) depth->V.Vector SA.Contour->V.Vector (V2 Float)
getFeatures canniedImg contours = CV.goodFeaturesToTrack canniedImg {-(fromIntegral (4*(V.length contours)))-} 1000 0.95 100 Nothing Nothing $ CV.HarrisDetector 0.2 --50 is the min distance between two features

     
getDiffVectorXY::V.Vector CV.Point2i->CV.Point2i->(V2 Int32 -> Int)->[Int]
getDiffVectorXY pts headPt getComp
    | V.length pts == 0 =  []
    | V.length pts == 1 =  [(getComp $ P.fromPoint headPt) - (getComp $ P.fromPoint $ pts V.! 0)]
    | otherwise         = [(getComp $ P.fromPoint $ pts V.! 1) - (getComp $ P.fromPoint $ pts V.! 0)] ++ (getDiffVectorXY (V.tail pts) headPt getComp)     

getDiffList::[Int32]->Int32->[Int32]
getDiffList pts headPt
    | length pts ==0 = []
    | length pts ==1 = [headPt - pts !! 0]
    | otherwise      = [pts !! 1 - pts !! 0]++getDiffList (tail pts) headPt

{-point2iToPointTuple::V.Vector CV.Point2i->[PointTuple]
point2iToPointTuple pts
    | V.length pts == 0 = []
    | V.length pts == 1 = [(getXComp (P.fromPoint (pts V.! 0)), getYComp (P.fromPoint (pts V.! 0)))]
    | otherwise         = [(getXComp (P.fromPoint (pts V.! 0)), getYComp (P.fromPoint (pts V.! 0)))] ++ (point2iToPointTuple $ V.tail pts) 
    -}