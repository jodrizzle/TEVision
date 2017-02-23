module Main where

import qualified Data.ByteString as B 
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified Data.Vector as V
import Control.Monad (void,when)
import Data.Function
import Data.List
import Data.Map
import Data.Maybe    
import Data.Proxy
import Foreign.C.Types
import GHC.Int (Int32)
import GHC.Word  
import Linear
import System.Environment

import Filters
import Transforms
import Utilities

main :: IO ()
main = do
      --Parse arguments-------------------------------------------------------------------------------
        args<-getArgs
        let fname = args !! 0 --filename is first argument
        
      --Read image------------------------------------------------------------------------------------
        imgOrig  <- CV.imdecode CV.ImreadUnchanged <$> B.readFile ("../data/"++fname)      
        imgGS    <- CV.imdecode CV.ImreadGrayscale <$> B.readFile ("../data/"++fname)
        
        showImage "Original" imgOrig
      --tighten matrix type constraints to work with canny and blur-----------------------------------
        let formImg = CV.exceptError $ M.coerceMat imgGS :: M.Mat ('CV.S '[ 'CV.D, 'CV.D]) ('CV.S 1) ('CV.S Word8)
        let canniedImg = cannyImg (gaussianBlurImg formImg 3)
      
      --detect and draw contours----------------------------------------------------------------------
        contours <- (getContours canniedImg)
        let simpleContours = V.map CV.contourPoints contours --contours without children, i.e. Vector of Vectors of Point2i
        let areas = getAreas simpleContours
        let largestIndex = findLargestContourIndex areas
        let peri = CV.exceptError $ CV.arcLength (CV.contourPoints $ contours V.! largestIndex) True   
        approxPolyContour <- CV.approxPolyDP (CV.contourPoints $ contours V.! largestIndex) (0.02*peri) True
        imgMut <- CV.thaw imgOrig--make mutable matrix  
        CV.drawContours (V.map CV.contourPoints contours) red (CV.OutlineContour CV.LineType_8 2) imgMut --action to mutate imgMut
        contoured_img <- CV.freeze imgMut--make matrix immutable again
        putStrLn $ "Number of outlines detected:\t" ++ (show $ V.length contours) --print length of vector of contours "how many contours detected?"
        showImage "Contours" contoured_img             
        showDetectedObjects (1) (V.singleton (CV.Contour approxPolyContour (CV.contourChildren $ contours V.! largestIndex)))  imgOrig formImg

showImage::String-> M.Mat (CV.S '[height, width]) channels depth-> IO ()
showImage title img = CV.withWindow title $ \window -> do  --display image
                        CV.imshow window img
                        CV.resizeWindow window 500 500
                        void $ CV.waitKey 100000
                        
showDetectedObjects::Int->(V.Vector CV.Contour)->M.Mat (CV.S '[height, width]) channels depth-> M.Mat ('CV.S '[ 'CV.D, 'CV.D]) ('CV.S 1) ('CV.S Word8)->IO ()
showDetectedObjects iter contours imgOrig imgGS
    | (V.null contours)   == True = putStrLn "NO OBJECTS DETECTED!"
    | otherwise                   = do
        let contour = CV.contourPoints $ contours V.! 0
        let a = orderPts contour
        let dims = (CV.fromSize  (CV.rectSize uprightBounder)::(V2 Int32))
        let srcVec = V.fromList [getPt 1 a, getPt 2 a, getPt 3 a, getPt 4 a]
        let dstVec = V.fromList [(V2 0 0),    (V2 (fromIntegral $ getXComp dims) 0),  (V2 0 (fromIntegral $ getYComp dims)) , (V2 (fromIntegral $ getXComp dims) (fromIntegral $ getYComp dims))]
        let t_pers = CV.getPerspectiveTransform (V.map (makePoint2f) srcVec) dstVec
        let uprightImg = perspectiveTransform imgGS t_pers
        let perimeter = CV.exceptError $ CV.arcLength contour True
        putStrLn $ "Perimeter of object "++show iter++":\t"++ show (round perimeter)
        showImage "Largest detected object" (cropImg uprightImg (V2 0 0) dims)
        showImage "Largest detected object" (threshBinary $ cropImg uprightImg (V2 0 0) dims)
        when (V.length contours > 1) $ showDetectedObjects (iter+1) (V.tail contours) imgOrig imgGS   
    where uprightBounder = getUprightBoundRect contours
        
showDimensions:: V2 Int32->Int-> IO ()
showDimensions dims iter = do
    putStrLn ("Height of matrix "++(show iter)++" (y_max): "++show  ((getYComp dims)))
    putStrLn ("Width of matrix  "++(show iter)++" (x_max): "++show  ((getXComp dims)))