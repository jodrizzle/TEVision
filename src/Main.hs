module Main where

import qualified Data.ByteString as B 
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified Data.Vector as V
import Control.Monad (void,when)
import Control.Monad.Primitive
import Data.Function
import Data.List
import Data.Map
import Data.Maybe    
import Data.Proxy
import Foreign.C.Types
import GHC.Int (Int32)
import GHC.Word  
import Linear
import System.Directory
import System.Exit

import Filters
import Transforms
import Utilities

main :: IO ()
main = do
      --Read images------------------------------------------------------------------------------------
        
        filePaths <- listDirectory "../data"
        let imgPaths = sort $ Data.List.filter isImgFile filePaths  
        createDirectoryIfMissing True "../data/Output"
        putStrLn $ "Image Files: \n"++show imgPaths
        if length imgPaths == 0
           then exitWith ExitSuccess
           else process imgPaths
        
           
process::[FilePath]->IO ()
process files = do 
        imgOrig  <- CV.imdecode CV.ImreadUnchanged <$> B.readFile ("../data/"++ (head files))      
        imgGS    <- CV.imdecode CV.ImreadGrayscale <$> B.readFile ("../data/"++ (head files))
        
      --tighten matrix type constraints to work with canny and blur-----------------------------------
        let formImg = CV.exceptError $ M.coerceMat imgGS :: M.Mat ('CV.S '[ 'CV.D, 'CV.D]) ('CV.S 1) ('CV.S Word8)
        let canniedImg = cannyImg (gaussianBlurImg formImg 3    )
        
      --detect and draw contours----------------------------------------------------------------------
        contours <- (getContours canniedImg)
        appConts <- approximateContours contours
        let quadrilaterals = V.filter isLarge $ V.filter isQuad appConts -- Vector of Vector of Points
        let largestIndex = findLargestContourIndex $ getAreas quadrilaterals
        putStrLn $ "\nNumber of documents detected in "++head files++":  " ++ (show $ V.length quadrilaterals) 
        if (V.length quadrilaterals == 0 && length files > 1)
           then process (tail files)
           else if (V.length quadrilaterals == 0 && length files ==1)
                    then exitWith ExitSuccess
                    else return ()
        imgMut <- CV.thaw imgOrig
        CV.drawContours (quadrilaterals) red (CV.OutlineContour CV.LineType_8 10) imgMut --action to mutate imgMut  
        --if (largestIndex /= (-1)) 
        saveDetectedObjects (1) quadrilaterals imgOrig formImg $ head files
        --    else return ()
        if (length files > 1)
            then process (tail files)
            else do
                putStrLn "\nDONE!"
                exitWith ExitSuccess
        
showImage::String-> M.Mat (CV.S '[height, width]) channels depth-> IO ()
showImage title img = CV.withWindow title $ \window -> do  --display image
                        CV.imshow window img
                        CV.resizeWindow window 500 500
                        void $ CV.waitKey 100000
                        
saveDetectedObjects::Int->(V.Vector (V.Vector CV.Point2i))->M.Mat (CV.S '[height, width]) channels depth-> M.Mat ('CV.S '[ 'CV.D, 'CV.D]) ('CV.S 1) ('CV.S Word8)->String->IO ()
saveDetectedObjects iter contours imgOrig imgGS fname
    | (V.null contours)   == True = putStrLn "NO OBJECTS DETECTED!"
    | otherwise                   = do
        let contour = contours V.! 0
        let a = orderPts contour
        let dims = (CV.fromSize  (CV.rectSize uprightBounder)::(V2 Int32))
        let srcVec = V.fromList [getPt 1 a, getPt 2 a, getPt 3 a, getPt 4 a]
        let dstVec = V.fromList [(V2 0 0),    (V2 (fromIntegral $ getXComp dims) 0),  (V2 0 (fromIntegral $ getYComp dims)) , (V2 (fromIntegral $ getXComp dims) (fromIntegral $ getYComp dims))]
        let t_pers = CV.getPerspectiveTransform (V.map (makePoint2f) srcVec) dstVec
        let uprightImg = perspectiveTransform imgGS t_pers
        B.writeFile ("../data/Output/"++fnameNoExt++"_"++show iter++".bmp") $ CV.exceptError $ CV.imencode CV.OutputBmp $ threshBinary $ cropImg uprightImg (V2 0 0) dims
        putStrLn $ "Wrote to file: ../data/Output/"++fnameNoExt++"_"++show iter++".bmp"      
        when (V.length contours > 1) $ saveDetectedObjects (iter+1) (V.tail contours) imgOrig imgGS fname
    where uprightBounder = getUprightBoundRect contours
          peri = CV.exceptError $ CV.arcLength (contours V.! 0) True
          fnameNoExt = reverse $ drop 4 $ reverse fname
          
showDimensions:: V2 Int32->Int-> IO ()
showDimensions dims iter = do
    putStrLn ("Height of matrix "++(show iter)++" (y_max): "++show  ((getYComp dims)))
    putStrLn ("Width of matrix  "++(show iter)++" (x_max): "++show  ((getXComp dims)))
