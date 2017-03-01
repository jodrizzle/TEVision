module Main where

import qualified Data.ByteString as B 
import OpenCV
import qualified Data.Vector as V
import Control.Monad (void,when)
import Control.Monad.Primitive
import Data.Function
import Data.List
import Data.Map
import Data.Maybe    
import Foreign.C.Types
import GHC.Int (Int16,Int32)
import GHC.Word  
import Linear
import System.Directory
import System.Environment
import System.Exit

import Filters
import Transforms
import Utilities

main :: IO ()
main = do
      --Read images------------------------------------------------------------------------------------
        args <- getArgs
        if (length args > 0) 
           then do
               let fname = args !! 0 --file specified
               process [fname]
           else do                   --no file specified,do all
                filePaths <- listDirectory "../data/"
                let imgPaths = sort $ Data.List.filter isImgFile filePaths --sort and filter - only image files  
                createDirectoryIfMissing True "../data/Output"
                putStrLn $ "Image Files: \n"++show imgPaths
                if length imgPaths == 0
                    then exitWith ExitSuccess --no images found
                    else process imgPaths --process images
                    
process::[FilePath]->IO ()
process files = do   
        imgOrig  <- imdecode ImreadUnchanged <$> B.readFile ("../data/"++ (head files))
        --imgGS    <- imdecode ImreadGrayscale <$> B.readFile ("../data/"++ (head files))
        
        --let formImg = exceptError $ coerceMat imgGS :: Mat (S [ D, D]) (S 1) (S Word8)
        let formImgOrig = exceptError $ coerceMat imgOrig :: Mat (S [ D, D]) (S 3) (S Word8)  --tighten matrix type constraints to work with canny and blur  
        sharpImg <- enhanceEdges $ gaussianBlurImg formImgOrig 3 --blur and sharpen formatted image
        let edgeImg = cannyImg $ gaussianBlurImg (medianBlurImg (closingImg $ openingImg sharpImg) 3) 7 --open,close,blur and canny edge detection
        tmp' <- morphImg edgeImg MorphGradient 15 --do gradient detection (15 iterations)    
        
        --detect and draw contours----------------------------------------------------------------------
        putStrLn "This fails for .tif:"
        --contours  <- getContours edgeImg --(closingImg edgeImg)
        contours' <- getContours tmp' --get contours
        --rawConts  <- rawContours contours  
        rawConts' <- rawContours contours' -- contours with children removed - flattened contour hierarchy  
        --appConts  <- approximateContours contours
        appConts' <- approximateContours contours' -- polygonal approximation of contours
        --let quadrilaterals  = V.filter isLarge $ V.filter isQuad appConts -- Vector of Vector of Points
        let quadrilaterals' = V.filter isLarge $ V.filter isQuad appConts' -- large enough contours with four vertices - candidates for documents

        -- putStrLn $ "Number of documents detected in original image: " ++head files++":  " ++ (show $ V.length quadrilaterals) 
        putStrLn $ "Number of documents detected in closed   image:"  ++head files++":  " ++ (show $ V.length quadrilaterals')
        
        if          (V.length quadrilaterals' <= 0 && length files > 1) -- process next image if no outlines and there are more images waiting 
            then    process (tail files)
        else if     (V.length quadrilaterals' >  0 && length files > 1) -- save objects and process next image if outlines and there are more images waiting 
            then do
                    saveDetectedObjects (1) quadrilaterals' formImgOrig imgOrig $ head files
                    process (tail files)
        else if     (V.length quadrilaterals' > 0)                                                               
            then do
                    saveDetectedObjects (1) quadrilaterals' formImgOrig imgOrig $ head files
                    putStrLn "\nDONE!"
                    exitWith ExitSuccess                                      -- no more images remaining    
        else        exitWith ExitSuccess                                      --terminate if this was the last image
        
        --showImage "Canny before closing" edgeImg
        --tmp <- morphImg edgeImg MorphClose 5
        --tmp'' <- morphImg edgeImg MorphClose 15
        --showImage "Canny after closing" tmp
        --showImage "Canny after closing,grad" tmp'
        --showImage "Canny after closing,grad,closeing" tmp''
        --showImage "Erode" $ erodeImg 5 sharpImg
        --showImage "Dilate canny" $ closingImg $ cannyImg $ gaussianBlurImg (medianBlurImg (closingImg $ openingImg sharpImg) 3) 7
        --showImage "Orig" imgOrig
        --showImage "Sharpen after blur: Sharp <- gauss <- form <- orig" sharpImg
        --showImage "Sharpen no blur: Sharp <- gauss <- form <- orig" sharpImg'
        --showImage "Opening <- sharp <- gauss <- form <- orig" $ openingImg sharpImg
        --showImage "Closing <- opening <- sharp <- gauss <- form <- orig" $ closingImg $ openingImg sharpImg
        --showImage "Gauss <- closing <- opening <- sharp <- gauss <- form <- orig"  $ gaussianBlurImg (closingImg $ openingImg sharpImg) 9
        --showImage "Canny with preblur<- Gauss <- closing <- opening <- sharp <- gauss <- form <- orig" edgeImg
        --showImage "Canny no preblur<- Gauss <- closing <- opening <- sharp <- gauss <- form <- orig" edgeImg'
        
        --imgM     <- thaw imgOrig
        --drawContours (rawConts)  green (OutlineContour LineType_8 5) imgM --action to mutate imgMut  
        --drawContours (rawConts') red   (OutlineContour LineType_8 5) imgM --action to mutate imgMut  
        --imgCont  <-freeze imgM
        --showImage "Contours" imgCont
       
        --imgMut   <- thaw imgOrig
        --dawContours (quadrilaterals)  green (OutlineContour LineType_8 5) imgMut --action to mutate imgMut  
        --drawContours (quadrilaterals') red   (OutlineContour LineType_8 5) imgMut --action to mutate imgMut  
        --imgContA <-freeze imgMut
        --showImage "Approximated contours" imgContA 
          
showImage::String-> Mat (S [height, width]) channels depth-> IO ()
showImage title img = withWindow title $ \window -> do  --display image
                        imshow window img
                        resizeWindow window 1200 700 
                        void $ waitKey 100000
                        
saveDetectedObjects::Int->(V.Vector (V.Vector Point2i))-> Mat (S [ D, D]) (S 3) (S Word8)-> Mat (S [ D, D]) (D) (D)->String->IO ()
saveDetectedObjects iter contours imgGS imgOrig fname
    | (V.null contours)   == True = putStrLn "NO OBJECTS DETECTED!"
    | otherwise                   = do
        let contour = contours V.! 0
        let a = orderPts contour
        let dims = (fromSize  (rectSize uprightBounder)::(V2 Int32))
        let srcVec = V.fromList [getPt 1 a, getPt 2 a, getPt 3 a, getPt 4 a]
        let dstVec = V.fromList [(V2 0 0),    (V2 (fromIntegral $ getXComp dims) 0),  (V2 0 (fromIntegral $ getYComp dims)) , (V2 (fromIntegral $ getXComp dims) (fromIntegral $ getYComp dims))]
        let t_pers = getPerspectiveTransform (V.map (makePoint2f) srcVec) dstVec
        let uprightImg = perspectiveTransform imgGS t_pers
        --showImage "Corrected: " $ cropImg uprightImg (V2 0 0) dims
        --putStrLn $ "dims"++show dims
        --putStrLn $ "fname"++fname
        B.writeFile ("../data/Output/"++fnameNoExt++"_"++show iter++".tif") $ exceptError $ imencode OutputTiff  $ cropImg uprightImg (V2 0 0) dims
        putStrLn $ "Wrote to file: ../data/Output/"++fnameNoExt++"_"++show iter++".tif" 
        when (V.length contours > 1) $ saveDetectedObjects (iter+1) (V.tail contours)  imgGS imgOrig fname
    where uprightBounder = getUprightBoundRect $ contours V.! 0
          peri = exceptError $ arcLength (contours V.! 0) True
          fnameNoExt = reverse $ drop 4 $ reverse fname
          
showDimensions:: V2 Int32->Int-> IO ()
showDimensions dims iter = do
    putStrLn ("Height of matrix "++(show iter)++" (y_max): "++show  ((getYComp dims)))
    putStrLn ("Width of matrix  "++(show iter)++" (x_max): "++show  ((getXComp dims)))
