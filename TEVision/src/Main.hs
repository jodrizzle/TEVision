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
        
        showImage "Original" imgOrig
      --tighten matrix type constraints to work with canny and blur-----------------------------------
        let formImg = CV.exceptError $ M.coerceMat imgGS :: M.Mat ('CV.S '[ 'CV.D, 'CV.D]) ('CV.S 1) ('CV.S Word8)
        let kernel = getKernel blurRegion
        let canniedImg = cannyImg (gaussianBlurImg formImg kernel)
      
      --detect and draw contours----------------------------------------------------------------------
        contours <- (getContours canniedImg)
        imgMut <- CV.thaw imgOrig--make mutable matrix  
        CV.drawContours (V.map SA.contourPoints contours) red (CV.OutlineContour CV.LineType_8 2) imgMut --action to mutate imgMut
        contoured_img <- CV.freeze imgMut--make matrix immutable again
        putStrLn $ "Number of outlines detected:\t" ++ (show $ V.length contours) --print length of vector of contours "how many contours detected?".  One contour consists of many points
        --putStrLn $ show $ SA.contourPoints $ contours V.! 0
        let points = (SA.contourPoints (contours V.! 0))
        --putStrLn $ show $ V.length points
        let diffVecX = getDiffVectorXY points (V.head points) getXComp
        let diffVecY = getDiffVectorXY points (V.head points) getYComp
        putStrLn $ "Smoothed diffX:\n" ++ show (medianFilter1D diffVecX 2)
        let pt1 = points V.! 0 --first point in vector is a contour corner
        let pt2 = points V.! (getCorner 1 diffVecX)
        let pt3 = points V.! (2*(V.length points)`div`4)
        let pt4 = V.last points
        
        mutImg <-CV.thaw imgOrig
        CV.circle mutImg (pt1) 20 blue (-1) CV.LineType_AA 0
        CV.circle mutImg (pt2) 20 blue (-1) CV.LineType_AA 0
        CV.circle mutImg (pt3) 20 blue (-1) CV.LineType_AA 0
        CV.circle mutImg (pt4) 20 blue (-1) CV.LineType_AA 0
        circled_img <- CV.freeze mutImg
        showImage "Circles" circled_img
        
        --showImage "Edges (Gaussian blur)" canniedImg
        showImage "Contours" contoured_img
      
      --display results-------------------------------------------------------------------------------
      --showImage "Warped" $ warpAffineImg imgOrig --transform
      --showImage "Warped" $ warpAffineInvImg $ warpAffineImg imgOrig --apply inverse transform on transformed image
      --showImage "Laplacian" $ laplacianFilter (gaussianBlurImg formImg kernel)
      --showImage "Grayscale" imgGS
      --showImage "Edges (no prefilter)" $ cannyImg formImg
      --showImage "Edges (median blur)" $ cannyImg (medianBlurImg formImg kernel)
        showDetectedObjects (1) contours imgOrig
     
medianFilter1D::[Int32]->Int->[Int32] --  start index at 2
medianFilter1D diffs ix 
    | (length diffs < 5)                = diffs 
    | (ix>((length diffs - 1)-2))       = diffs
    | otherwise                         = prePart++[medianWndw]++postPart
    where prePart    = (take ix diffs)
          medianWndw = medianReplace ([diffs !! (ix-2)] ++ [diffs !! (ix-1)] ++ [diffs !! ix] ++ [diffs !! (ix+1)] ++ [diffs !! (ix+2)])
          postPart   = drop (ix+1) (medianFilter1D diffs (ix+1))    
    
medianReplace::[Int32]->Int32
medianReplace lst = (sort lst) !! 2

--get features
{-let features = getFeatures canniedImg contours
mutImg <-CV.thaw imgOrig
CV.circle mutImg (round <$> (features V.! 0)::V2 Int32) 20 blue (-1) CV.LineType_AA 0
CV.circle mutImg (round <$> (features V.! 1)::V2 Int32) 20 blue (-1) CV.LineType_AA 0
--overlayCircles mutImg features $ V.length features
circled_img <- CV.freeze mutImg
putStrLn $ "Number of features detected: "++(show $ V.length features)
showImage "Circles" circled_img
-}
{-CV.withMatM (Proxy :: Proxy [height, width]) 
(Proxy :: Proxy channels) 
(Proxy :: Proxy depth)
white $ imgM -> do
void $ CV.matCopyToM imgM (V2 0 0) imgOrig Nothing 
forM_ features $ f -> do
CV.circle imgM (round <$> f :: V2 Int32) 2 blue 5 LineType_AA 0-}
{-overlayCircles:: (PrimMonad m)=>m (CV.Mutable a (PrimState m))->V.Vector (V2 Float)->Int->m ()
overlayCircles imgM feats numFeats
| numFeats==0 = error "No features to draw!"
| numFeats==1 = CV.circle imgM (round <$> (feats V.! 0)::V2 Int32) 20 blue (-1) CV.LineType_AA 0
| otherwise   = do
CV.circle imgM (round <$> (feats V.! 0)::V2 Int32) 20 blue (-1) CV.LineType_AA 0
overlayCircles imgM (V.tail feats) (numFeats-1)-}