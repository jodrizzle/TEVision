module Utilities (  
    cropImg
   ,getContours
   ,inContour 
   ,laplacianFilter 
   ,rotationMatrix
   ,showImage
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

subRect= CV.toRect $ CV.HRect (V2 50 131) (V2 190 60)
      
cropImg::M.Mat (CV.S '[height, width]) channels depth -> (M.Mat (CV.S '[CV.D, CV.D]) channels depth)
cropImg img =  CV.exceptError $ CV.matSubRect img subRect        

getContours :: PrimMonad m => M.Mat ('CV.S '[h0, w0]) ('CV.S 1) ('CV.S Word8) -> m (V.Vector SA.Contour)
getContours image = do
                    imageM <- CV.thaw image
                    contours_vector <- CV.findContours SA.ContourRetrievalExternal SA.ContourApproximationTC89KCOS imageM
                    pure contours_vector
                    
inContour:: (CV.IsPoint2 contourPoint2 CFloat, CV.IsPoint2 testPoint2 CFloat)=> V.Vector (contourPoint2 CFloat)->testPoint2 CFloat -> Bool
inContour cont pt
    | (CV.exceptError $ SA.pointPolygonTest cont pt False) >=0 = True
    | otherwise = False
    
--has to be grayscale input                    
laplacianFilter::M.Mat shape ('CV.S 1) ('CV.S Word8)-> M.Mat shape ('CV.S 1) ('CV.S Word8)
laplacianFilter img = CV.exceptError $ CV.laplacian Nothing Nothing Nothing Nothing img
        
perspectiveTransform:: M.Mat ('CV.S '[height, width]) channels depth -> M.Mat (M.ShapeT '[3,3]) (CV.S 1) (CV.S Double)-> M.Mat ('CV.S '[height, width]) channels depth
perspectiveTransform img t  = CV.exceptError $ CV.warpPerspective img t CV.InterNearest False True CV.BorderReplicate

rotationMatrix :: M.Mat (M.ShapeT [2, 3]) ('CV.S 1) ('CV.S Double)
rotationMatrix = CV.getRotationMatrix2D (V2 256 170 ) 30 0.6

showImage::String-> M.Mat (CV.S '[height, width]) channels depth-> IO ()
showImage title img = CV.withWindow title $ \window -> do  --display image
                        CV.imshow window img
                        CV.resizeWindow window 500 500
                        void $ CV.waitKey 100000

warpAffineImg :: M.Mat (CV.S '[height, width]) channels depth->(M.Mat (CV.S '[height, width]) channels depth)
warpAffineImg img = CV.exceptError $ CV.warpAffine img rotationMatrix CV.InterArea False False (CV.BorderConstant black)

warpAffineInvImg ::  M.Mat (CV.S '[height, width]) channels depth->(M.Mat (CV.S '[height, width]) channels depth)
warpAffineInvImg img= CV.exceptError $ CV.warpAffine img rotationMatrix CV.InterCubic True False (CV.BorderConstant black)







-- maskFunction::[Int] -> Int -> M.StaticDepthT depth
-- maskFunction [y, x] 0
--     | (inContour $ P.toPoint (V2 x y :: V2 CFloat) CFloat) == True = [y, x]
--     | otherwise = [0, 0]
-- maskFunction [y, x] 1  
--     | (inContour $ P.toPoint (V2 x y :: V2 CFloat) CFloat) == True = [y, x]
--     | otherwise = [0, 0]
-- maskFunction _pos _channel =  error "impossible"
-- 
-- remapImg:: V.Vector SA.Contour -> M.Mat ('CV.S ['CV.S height, 'CV.S width]) ('CV.S channels) ('CV.S depth) -> M.Mat ('CV.S ['CV.S height, 'CV.S width]) ('CV.S channels) ('CV.S depth)
-- remapImg contours img = CV.exceptError $ CV.remap img (CV.matFromFunc (Proxy :: Proxy [height, width]) (Proxy :: Proxy 2) (Proxy :: Proxy Float) maskFunction) CV.InterLinear (CV.BorderConstant black)