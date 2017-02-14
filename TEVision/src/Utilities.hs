module Utilities (  
    cropImg
   ,getUprightBoundRect 
   ,getContours
   ,inContour 
   ,rotationMatrix
   ,showImage
   ,showDetectedObjects
   ,warpAffineImg
   ,warpAffineInvImg
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

subRect:: (V2 Int32)->(V2 Int32)->CV.Rect Int32
subRect topleft sz = CV.toRect $ CV.HRect topleft sz  --topleft (dist from left, dist from top), rect-size (width, height)

cropImg:: M.Mat (CV.S '[height, width]) channels depth -> (V2 Int32) -> (V2 Int32) -> (M.Mat (CV.S '[CV.D, CV.D]) channels depth)
cropImg img topleft sz=  CV.exceptError $ CV.matSubRect img $ subRect topleft sz   

getContours :: PrimMonad m => M.Mat ('CV.S '[h0, w0]) ('CV.S 1) ('CV.S Word8) -> m (V.Vector SA.Contour)
getContours image = do
                    imageM <- CV.thaw image
                    contours_vector <- CV.findContours SA.ContourRetrievalExternal SA.ContourApproximationTC89KCOS imageM
                    pure contours_vector
                    
inContour:: (CV.IsPoint2 contourPoint2 CFloat, CV.IsPoint2 testPoint2 CFloat)=> V.Vector (contourPoint2 CFloat)->testPoint2 CFloat -> Bool
inContour cont pt
    | (CV.exceptError $ SA.pointPolygonTest cont pt False) >=0 = True
    | otherwise = False
        
perspectiveTransform:: M.Mat ('CV.S '[height, width]) channels depth -> M.Mat (M.ShapeT '[3,3]) (CV.S 1) (CV.S Double)-> M.Mat ('CV.S '[height, width]) channels depth
perspectiveTransform img t  = CV.exceptError $ CV.warpPerspective img t CV.InterNearest False True CV.BorderReplicate

rotationMatrix :: M.Mat (M.ShapeT [2, 3]) ('CV.S 1) ('CV.S Double)
rotationMatrix = CV.getRotationMatrix2D (V2 256 170 ) 30 0.6

showImage::String-> M.Mat (CV.S '[height, width]) channels depth-> IO ()
showImage title img = CV.withWindow title $ \window -> do  --display image
                        CV.imshow window img
                        CV.resizeWindow window 500 500
                        void $ CV.waitKey 100000
                        
showDetectedObjects::Int->(V.Vector SA.Contour)->M.Mat (CV.S '[height, width]) channels depth->IO ()
showDetectedObjects iter contours imgOrig
    | (V.null contours)   == True = error "NO OBJECTS DETECTED!"
    | (V.length contours) == 1    = showImage ("Object "++(show iter)) (cropImg imgOrig (CV.fromPoint (CV.rectTopLeft uprightBounder)::(V2 Int32)) (CV.fromSize  (CV.rectSize uprightBounder)::(V2 Int32)))
    | otherwise                   = do
                                    showImage ("Object "++(show iter)) (cropImg imgOrig (CV.fromPoint (CV.rectTopLeft uprightBounder)::(V2 Int32)) (CV.fromSize  (CV.rectSize uprightBounder)::(V2 Int32)))
                                    showDetectedObjects (iter+1) (V.tail contours) imgOrig
    where uprightBounder = getUprightBoundRect contours

warpAffineImg :: M.Mat (CV.S '[height, width]) channels depth->(M.Mat (CV.S '[height, width]) channels depth)
warpAffineImg img = CV.exceptError $ CV.warpAffine img rotationMatrix CV.InterArea False False (CV.BorderConstant black)

warpAffineInvImg ::  M.Mat (CV.S '[height, width]) channels depth->(M.Mat (CV.S '[height, width]) channels depth)
warpAffineInvImg img= CV.exceptError $ CV.warpAffine img rotationMatrix CV.InterCubic True False (CV.BorderConstant black)