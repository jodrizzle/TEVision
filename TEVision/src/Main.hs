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
import Filters
import GHC.Int (Int32)
import GHC.Word  
import Graphics.UI.GLFW
import Linear
import RightReceipt_CA (findCoords,getListOfPoints)
import System.Environment
import Utilities
import Utils_CA (dictAreaPoints)
import Foreign.C.Types
import OpenCV.Internal.C.Types

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
        --let red = CV.toScalar (V4   0   0 255 255 :: V4 Double)--color for drawContours
        CV.drawContours (V.map SA.contourPoints contours) red (CV.OutlineContour CV.LineType_AA 5) imgMut --action to mutate imgMut
        contoured_img <- CV.freeze imgMut--make matrix immutable again
      --putStrLn $ show $ V.length $ SA.contourPoints $ contours V.! 0 --first contour in contours (Vector of contours;each contour is a Vector of point2i)
        putStrLn $ "Number of outlines detected: " ++ (show $ V.length contours) --print length of vector of contours "how many contours detected?".  One contour consists of many points
      --putStrLn $ show $ SA.contourPoints $ contours V.! 0
        
      --let listOfPoints   = (V.toList contours)
      --putStrLn $ show $ listOfPoints
      --let listOfAreas    = dictAreaPoints listOfPoints
      --let screenContours = findCoords listOfAreas
      --putStrLn $ show rotationMatrix
      --putStrLn $ show $ inContour (SA.contourPoints $ contours V.! 0) $ P.toPoint (V2 0.0 0.0)        --Use matConvertTo to change depth
      --display results-------------------------------------------------------------------------------
        showImage "Original" $ imgOrig
      --showImage "Warped" $ warpAffineImg imgOrig --transform
      --showImage "Warped" $ warpAffineInvImg $ warpAffineImg imgOrig --apply inverse transform on transformed image
      --showImage "Laplacian" $ laplacianFilter (gaussianBlurImg formImg kernel)
      --showImage "Grayscale" imgGS
        showImage "Edges (no prefilter)" $ cannyImg formImg
      --showImage "Edges (median blur)" $ cannyImg (medianBlurImg formImg kernel)
        showImage "Edges (Gaussian blur)" canniedImg
        showImage "Contours" contoured_img

        showImage "Cropped" $ cropImg imgOrig
        

-- matFromFuncImg = CV.exceptError $ CV.matFromFunc 
--       (Proxy :: Proxy [size, size])
--       (Proxy :: Proxy 4)
--       (Proxy :: Proxy Word8)
--       example
--   where
--     example [y, x] 0 = 255 - normDist (V2 x y ^-^ bluePt )
--     example [y, x] 1 = 255 - normDist (V2 x y ^-^ greenPt)
--     example [y, x] 2 = 255 - normDist (V2 x y ^-^ redPt  )
--     example [y, x] 3 =       normDist (V2 x y ^-^ alphaPt)
--     example _pos _channel = error "impossible"
-- 
-- normDist :: V2 Int -> Word8
-- normDist v = floor $ min 255 $ 255 * Linear.norm (fromIntegral <$> v) / s'
-- 
-- bluePt  = V2 0 0
-- greenPt = V2 s s
-- redPt   = V2 s 0
-- alphaPt = V2 0 s
-- 
-- s = fromInteger $ natVal (size) :: Int
-- s' = fromIntegral s :: Double
