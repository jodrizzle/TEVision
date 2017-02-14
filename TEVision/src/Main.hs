module Main where

import qualified Data.ByteString as B 
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified OpenCV.Internal.Core.Types.Point as P
import qualified OpenCV.ImgProc.StructuralAnalysis as SA 
import qualified Data.Vector as V
import Control.Monad (void)
import Control.Monad.Primitive
import Data.Int
import Data.List
import Data.Map
import Data.Maybe    
import Data.Proxy
import GHC.Int (Int32)
import GHC.Word  
import Graphics.UI.GLFW
import Linear
import System.Environment
import Foreign.C.Types
import OpenCV.Internal.C.Types

import Filters
import ImageIO
import Transforms
import Utilities

main :: IO ()
main = do
      --Parse arguments-------------------------------------------------------------------------------
        args<-getArgs
        let fname = args !! 0 --filename is first argument
        let blurRegion = args !! 1 --blur kernel size is second argument
        
      --Read image------------------------------------------------------------------------------------
        imgOrig  <- CV.imdecode CV.ImreadUnchanged <$> B.readFile ("../data/"++fname)      
        imgGS    <- CV.imdecode CV.ImreadGrayscale <$> B.readFile ("../data/"++fname)
        
      --tighten matrix type constraints to work with canny and blur-----------------------------------
        let formImg = CV.exceptError $ M.coerceMat imgGS :: M.Mat ('CV.S '[ 'CV.D, 'CV.D]) ('CV.S 1) ('CV.S Word8)
        let kernel = getKernel blurRegion
        let canniedImg = cannyImg (gaussianBlurImg formImg kernel)
      
      --detect and draw contours----------------------------------------------------------------------
        contours <- (getContours canniedImg)
        imgMut <- CV.thaw imgOrig--make mutable matrix  
        CV.drawContours (V.map SA.contourPoints contours) red (CV.OutlineContour CV.LineType_AA 5) imgMut --action to mutate imgMut
        contoured_img <- CV.freeze imgMut--make matrix immutable again
        putStrLn $ "Number of outlines detected: " ++ (show $ V.length contours) --print length of vector of contours "how many contours detected?".  One contour consists of many points
        
      --display results-------------------------------------------------------------------------------
        showImage "Original" imgOrig
      --showImage "Warped" $ warpAffineImg imgOrig --transform
      --showImage "Warped" $ warpAffineInvImg $ warpAffineImg imgOrig --apply inverse transform on transformed image
      --showImage "Laplacian" $ laplacianFilter (gaussianBlurImg formImg kernel)
      --showImage "Grayscale" imgGS
      --showImage "Edges (no prefilter)" $ cannyImg formImg
      --showImage "Edges (median blur)" $ cannyImg (medianBlurImg formImg kernel)
      --showImage "Edges (Gaussian blur)" canniedImg
        showImage "Contours" contoured_img
        showDetectedObjects (1) contours imgOrig