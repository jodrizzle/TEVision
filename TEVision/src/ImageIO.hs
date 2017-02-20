module ImageIO(
    showImage
   ,showDetectedObjects
   ,showObjectsWithCorners
)   where
    
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified OpenCV.Internal.Core.Types.Point as P
import qualified OpenCV.ImgProc.StructuralAnalysis as SA 
import qualified Data.Vector as V
import Control.Monad (void,when)
import Control.Monad.Primitive
import Data.Function
import Data.List
import Data.Proxy
import Foreign.C.Types
import GHC.Int (Int32)
import GHC.Word 
import Linear
import OpenCV.Internal.C.Types
import OpenCV.TypeLevel
import System.Environment

import Filters
import Transforms
import Utilities

showImage::String-> M.Mat (CV.S '[height, width]) channels depth-> IO ()
showImage title img = CV.withWindow title $ \window -> do  --display image
                        CV.imshow window img
                        CV.resizeWindow window 500 500
                        void $ CV.waitKey 100000
                        
showDetectedObjects::Int->(V.Vector SA.Contour)->M.Mat (CV.S '[height, width]) channels depth->IO ()
showDetectedObjects iter contours imgOrig
    | (V.null contours)   == True = putStrLn "NO OBJECTS DETECTED!"
    | otherwise                   = do
        let dims = (CV.fromSize  (CV.rectSize uprightBounder)::(V2 Int32))
        let croppedImg = (cropImg imgOrig (CV.fromPoint (CV.rectTopLeft uprightBounder)::(V2 Int32)) dims) 
        showImage ("Object "++(show iter)) croppedImg
        showDimensions dims iter
      --  t_Matrix <- CV.newMatx33d 1.31678 0.116182 (-179.158) (-0.124742) 1.40184 0.0182986 0.000797832 0.000793051 0.643084
        t_Matrix <- CV.newMatx33d 1.31678 (-0.124742) 0.000797832 0.116182  1.40184 0.000793051 (-179.158)  0.0182986 0.643084
        let persT = CV.exceptError $ M.coerceMat (CV.toMat t_Matrix) :: M.Mat ('CV.S '[ 'CV.S 3, 'CV.S 3]) ('CV.S 1) ('CV.S Double) 
        showImage ("perspective corrected") (perspectiveTransform imgOrig persT)
        when (V.length contours > 1) $ showDetectedObjects (iter+1) (V.tail contours) imgOrig   
    where uprightBounder = getUprightBoundRect contours
          
showObjectsWithCorners::Int->(V.Vector SA.Contour)->M.Mat (CV.S '[height, width]) channels depth->IO ()
showObjectsWithCorners iter contours imgOrig
    | (V.null contours)   == True = putStrLn "NO OBJECTS DETECTED!"
    | otherwise                   = do
        let a  = orderPts $ getRectCorners $ findEnclosingRectangle $ SA.contourPoints $ contours V.! 0
        mutImg <-CV.thaw imgOrig
        putStrLn $ "Point 1: " ++ show (getPt 1 a)
        putStrLn $ "Point 2: " ++ show (getPt 2 a)
        putStrLn $ "Point 3: " ++ show (getPt 3 a)
        putStrLn $ "Point 4: " ++ show (getPt 4 a)
        CV.circle mutImg (P.toPoint (getPt 1 a)) 4 blue (-1) CV.LineType_AA 0
        CV.circle mutImg (P.toPoint (getPt 2 a)) 4 blue (-1) CV.LineType_AA 0
        CV.circle mutImg (P.toPoint (getPt 3 a)) 4 blue (-1) CV.LineType_AA 0
        CV.circle mutImg (P.toPoint (getPt 4 a)) 4 blue (-1) CV.LineType_AA 0
        circled_img <- CV.freeze mutImg
        if (V.length contours>1) 
           then showObjectsWithCorners (iter+1) (V.tail contours) circled_img 
           else showImage ("Corners of object "++show iter) circled_img

showDimensions:: V2 Int32->Int-> IO ()
showDimensions dims iter = do
    putStrLn ("Height of matrix "++(show iter)++" (y_max): "++show  ((getIntXComp dims) - 1))
    putStrLn ("Width of matrix  "++(show iter)++" (x_max): "++show  ((getIntYComp dims) - 1))