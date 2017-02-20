module Main where

import qualified Data.ByteString as B 
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified OpenCV.Internal.Core.Types.Point as P
import qualified OpenCV.ImgProc.StructuralAnalysis as SA 
import qualified Data.Vector as V
import Control.Monad (void)
import Control.Monad.Primitive
import Data.Bool
import Data.Int
import Data.List
import Data.Map
import Data.Maybe    
import Data.Proxy
import Foreign.C.Types
import GHC.Int (Int32)
import GHC.Word  
import Graphics.UI.GLFW
import Linear
import System.Environment
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
        putStrLn $ "Number of outlines detected:\t" ++ (show $ V.length contours) --print length of vector of contours "how many contours detected?"
        
        let points = (SA.contourPoints (contours V.! 0))
        let rotRect = findEnclosingRectangle points
        let a  = getRectCorners rotRect
        let p1 = getPt 1 a    
        let p2 = getPt 2 a
        let p3 = getPt 3 a
        let p4 = getPt 4 a
        
        
        mutImg <-CV.thaw imgOrig
        
        CV.circle mutImg (P.toPoint p1) 4 blue (-1) CV.LineType_AA 0
        CV.circle mutImg (P.toPoint p2) 4 blue (-1) CV.LineType_AA 0
        CV.circle mutImg (P.toPoint p3) 4 blue (-1) CV.LineType_AA 0
        CV.circle mutImg (P.toPoint p4) 4 blue (-1) CV.LineType_AA 0
        circled_img <- CV.freeze mutImg
        showImage "Contours" contoured_img
        showImage "Corners" circled_img
        
        --showImage "Edges (Gaussian blur)" canniedImg
      
      --display results-------------------------------------------------------------------------------
        showDetectedObjects (1) contours imgOrig

        
        
        
        
{-    putStrLn $ show $ V.length points
    let diffVecX = getDiffVectorXY points (V.head points) getXComp
    let diffVecXX = getDiffList diffVecX (head diffVecX) 
    let diffVecY = getDiffVectorXY points (V.head points) getYComp
    let diffVecYY = getDiffList diffVecY (head diffVecY) 
    let contourList =point2iToPointTuple (SA.contourPoints $ contours V.! 0) -- [(Int32, Int32)]
    let xtremePts = minifyPolygon contourList 0.02
    putStrLn $ "\nPoints in first contour:\n" ++ (show . length) xtremePts
    mapM_ (putStrLn . prettyPrint) xtremePts
    putStrLn $ "Smoothed diffX:\n" ++ show (medianFilter1DFine diffVecX 1)
    putStrLn $ "Smoothed diffY:\n" ++ show (medianFilter1DFine diffVecY 1)
    putStrLn $ "diffX:\n" ++ show diffVecX
    putStrLn $ "diffY:\n" ++ show diffVecY        

prettyPrint :: (PointTuple) -> String
prettyPrint (x, y) = "( "++(show x) ++ ", " ++ (show y)++" )\n"


findSequence::(Int numSimilar, Bool sign,Int indx,Bool sndSeq)=>[Int32]->numSimilar->sign->indx->sndSeq->Int32
findSequence [] _ _    = -1
findSequence [x] 3 sign
    | (sgn x==sign) && (not sndSeq) = findSequence 0 (not sign) (indx+1)
    | otherwise   = -1
--findSequence (x:xs) 4 sign =  

--False: (-)
--True:  (+)
getCorner::[Int32]->[Int32]->Int->Bool->Int->Bool->Int
getCorner x y signCount sign ix x_checked
    | ix>=length x                                                                               = -1
    | (signCount <  4) &&       (sameSign (x!!ix) sign) &&                   (x_checked==False)  = getCorner x y (signCount+1)     sign  (ix+1) False
    | (signCount <  4) && ((not (sameSign (x!!ix) sign)) || ((x!!ix)==0)) && (x_checked==False)  = getCorner x y            0 (not sign) (ix+1) False
    | (signCount >= 4) &&                                                    (x_checked==False)  = getCorner x y            0      True  (ix+1) True
    | (signCount <  4) &&       (sameSign (x!!ix) sign)                                          = getCorner x y (signCount+1)     sign  (ix+1) True
    | (signCount <  4) && ((not (sameSign (x!!ix) sign)) || ((x!!ix)==0))                        = getCorner x y            0 (not sign) (ix+1) True
    | (signCount >= 4) &&       (x_checked==True)                                                = ix-7
--four of same sign sequentially followed by four of opposite sign implies corner

--get features
let features = getFeatures canniedImg contours
mutImg <-CV.thaw imgOrig
CV.circle mutImg (round <$> (features V.! 0)::V2 Int32) 20 blue (-1) CV.LineType_AA 0
CV.circle mutImg (round <$> (features V.! 1)::V2 Int32) 20 blue (-1) CV.LineType_AA 0
--overlayCircles mutImg features $ V.length features
circled_img <- CV.freeze mutImg
putStrLn $ "Number of features detected: "++(show $ V.length features)
showImage "Circles" circled_img

CV.withMatM (Proxy :: Proxy [height, width]) 
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