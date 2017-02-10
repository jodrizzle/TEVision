module Utilities (
    getContours
   ,laplacianFilter 
   --,remapImage
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
import GHC.Int (Int32)
import GHC.Word 
import Linear
import System.Environment 
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified OpenCV.ImgProc.StructuralAnalysis as SA 
import qualified Data.Vector as V

transparent, white, black, blue, green, red :: CV.Scalar
transparent = CV.toScalar (V4 255 255 255   0 :: V4 Double)
white       = CV.toScalar (V4 255 255 255 255 :: V4 Double)
black       = CV.toScalar (V4   0   0   0 255 :: V4 Double)
blue        = CV.toScalar (V4 255   0   0 255 :: V4 Double)
green       = CV.toScalar (V4   0 255   0 255 :: V4 Double)
red         = CV.toScalar (V4   0   0 255 255 :: V4 Double)

getContours :: PrimMonad m => M.Mat ('CV.S '[h0, w0]) ('CV.S 1) ('CV.S Word8) -> m (V.Vector SA.Contour)
getContours image = do
                    imageM <- CV.thaw image
                    contours_vector <- CV.findContours SA.ContourRetrievalExternal SA.ContourApproximationTC89KCOS imageM
                    pure contours_vector

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
                        void $ CV.waitKey 100000

warpAffineImg :: M.Mat (CV.S '[height, width]) channels depth->(M.Mat (CV.S '[height, width]) channels depth)
warpAffineImg img = CV.exceptError $ CV.warpAffine img rotationMatrix CV.InterArea False False (CV.BorderConstant black)

warpAffineInvImg ::  M.Mat (CV.S '[height, width]) channels depth->(M.Mat (CV.S '[height, width]) channels depth)
warpAffineInvImg img= CV.exceptError $ CV.warpAffine img rotationMatrix CV.InterCubic True False (CV.BorderConstant black)