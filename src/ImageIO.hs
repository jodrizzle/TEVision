module ImageIO(
    showImage
   ,showDetectedObjects
   --   ,showObjectsWithCorners
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
import Math.LinearEquationSolver
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
                        
showDetectedObjects::Int->(V.Vector SA.Contour)->M.Mat (CV.S '[height, width]) channels depth-> M.Mat ('CV.S '[ 'CV.D, 'CV.D]) ('CV.S 1) ('CV.S Word8)->IO ()
showDetectedObjects iter contours imgOrig imgGS
    | (V.null contours)   == True = putStrLn "NO OBJECTS DETECTED!"
    | otherwise                   = do
        let contour = SA.contourPoints $ contours V.! 0
        putStrLn $ "Points:\t\t"++show contour
        let a  = orderPts $ getRectCorners $ findEnclosingRectangle contour
        let dims = (CV.fromSize  (CV.rectSize uprightBounder)::(V2 Int32))
        
        let a' = orderPts' contour
        --let dims' = P.fromPoint (a' V.! 3)
        
        let srcVec = V.fromList [getPt 1 a', getPt 2 a', getPt 3 a', getPt 4 a']
        let dstVec = V.fromList [(V2 0 0),    (V2 (fromIntegral $ getIntXComp dims) 0),  (V2 0 (fromIntegral $ getIntYComp dims)) , (V2 (fromIntegral $ getIntXComp dims) (fromIntegral $ getIntYComp dims))]
        let t_pers = CV.getPerspectiveTransform (V.map (makePoint2f) srcVec) dstVec
        let uprightImg = perspectiveTransform imgGS t_pers
        let perimeter = CV.exceptError $ CV.arcLength contour True
        putStrLn $ "Perimeter of object "++show iter++":\t"++ show (round perimeter)
        showImage ("perspective corrected and cropped, perimeter is "++show (round perimeter)) (threshBinary $ cropImg uprightImg (V2 0 0) dims)
        when (V.length contours > 1) $ showDetectedObjects (iter+1) (V.tail contours) imgOrig imgGS   
    where uprightBounder = getUprightBoundRect contours
        
{-showObjectsWithCorners::Int->(V.Vector SA.Contour)->M.Mat (CV.S '[height, width]) channels depth->IO ()
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
           else showImage ("Corners of object "++show iter) circled_img-}

showDimensions:: V2 Int32->Int-> IO ()
showDimensions dims iter = do
    putStrLn ("Height of matrix "++(show iter)++" (y_max): "++show  ((getIntYComp dims)))
    putStrLn ("Width of matrix  "++(show iter)++" (x_max): "++show  ((getIntXComp dims)))