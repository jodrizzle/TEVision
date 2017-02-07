module Main where

import qualified Data.ByteString as B 
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified OpenCV.ImgProc.StructuralAnalysis as SA 
import qualified Data.Vector as V
import Control.Monad (void)
import Control.Monad.Primitive
import Data.List
import Data.Map
import Data.Maybe   
import GHC.Int (Int32)
import GHC.Word  
import Linear
import System.Environment 
import Filters
import Utilities

main :: IO ()
main = do
        --Parse arguments
        args<-getArgs
        let fname = args !! 0 --filename is first argument
        let blurRegion = args !! 1 --blur kernel size is second argument
        
        --Read image
        imgOrig  <- CV.imdecode CV.ImreadUnchanged <$> B.readFile ("../data/"++fname)      
        imgGS    <- CV.imdecode CV.ImreadGrayscale <$> B.readFile ("../data/"++fname)
        
        --tighten matrix type constraints to work with canny and blur
        let formImg = M.unsafeCoerceMat imgGS :: M.Mat ('CV.S '[ 'CV.D, 'CV.D]) ('CV.S 3) ('CV.S Word8)
        let kernel = getKernel blurRegion
        let canniedImg = cannyImg (gaussianBlurImg formImg kernel)
        
        --detect contours
        contours <- (findingContours canniedImg)
        imgMut <- CV.thaw imgOrig--make mutable matrix 
        let red = CV.toScalar (V4   0   0 255 255 :: V4 Double)--color for drawContours
        draw_on_imgMut <- CV.drawContours (V.map SA.contourPoints contours) red (CV.OutlineContour CV.LineType_AA 1) imgMut --action to mutate imgMut
        contoured_img <- CV.freeze imgMut--make matrix immutable again
        
        --display results
        showImage "Original" imgOrig
        --showImage "Grayscale" imgGS
        showImage "Edges (no prefilter)" $ cannyImg formImg
        --showImage "Edges (median blur)" $ cannyImg (medianBlurImg formImg kernel)
        showImage "Edges (Gaussian blur)" canniedImg
        showImage "Contours" contoured_img