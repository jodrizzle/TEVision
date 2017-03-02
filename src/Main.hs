module Main where

import qualified Data.ByteString as B 
import qualified Data.Vector as V
import Control.Monad (void,when)
import Data.List   
import GHC.Word  
import OpenCV
import System.Directory
import System.Environment
import System.Exit

import Filters
import Transforms
import Utilities

main :: IO ()
main = do
        args <- getArgs
        if (length args > 0) 
           then do
               let fname = args !! 0 --file specified
               process [fname]
           else do                   --no file specified,do all
                filePaths <- listDirectory "../data/"
                let imgPaths = sort $ filter isImgFile filePaths --sort and filter - only image files  
                createDirectoryIfMissing True "../data/Output"
                putStrLn $ "Image Files: \n"++show imgPaths++"\n"
                if length imgPaths == 0
                    then exitWith ExitSuccess --no images found
                    else process imgPaths --process images

process::[FilePath]->IO ()
process files = do           
        img <- imdecode ImreadGrayscale <$> B.readFile ("../data/"++ (head files))
        let formimg = exceptError $ coerceMat img :: Mat (S [ D, D]) (S 1) (S Word8)
        sharpImg <- (enhanceEdges $ gaussianBlurImg formimg 7) >>= (morphImg MorphOpen 1) >>= (morphImg MorphClose 1) -- blur and sharpen formatted image
        thickEdges <- morphImg MorphGradient 2 $ cannyImg sharpImg -- do gradient detection (2 iterations)    
        contours <- getContours thickEdges --get contours
        rawConts <- rawContours contours -- contours with children removed - flattened contour hierarchy  
        appConts <- approximateContours contours -- polygonal approximation of contours
        let quadrilaterals = V.filter isSimplePolygon $ V.filter isLong $ V.filter isQuad appConts -- large enough contours with four vertices - candidates for documents
        if          (V.length quadrilaterals <= 0 && length files > 1) -- process next image if no outlines and there are more images waiting 
            then    process (tail files)
        else if     (V.length quadrilaterals >  0 && length files > 1) -- save objects and process next image if outlines and there are more images waiting 
            then do
                    saveDetectedObjects (1) quadrilaterals img $ head files
                    process (tail files)
        else if     (V.length quadrilaterals > 0)                                                               
            then do
                    saveDetectedObjects (1) quadrilaterals img $ head files
                    putStrLn "\nDONE!"
                    exitWith ExitSuccess                                      -- no more images remaining    
        else        exitWith ExitSuccess                                      --terminate if this was the last image

showImage::String-> Mat (S [height, width]) channels depth-> IO ()
showImage title img = withWindow title $ \window -> do  --display image
                        imshow window img
                        resizeWindow window 1200 700 
                        void $ waitKey 100000
                        
saveDetectedObjects::Int->(V.Vector (V.Vector Point2i))-> Mat (S [ D, D]) (D) (D)->String->IO ()
saveDetectedObjects iter contours img fname
    | (V.null contours)   == True = putStrLn "NO OBJECTS DETECTED!"
    | otherwise                   = do
        let correctedImg = correctImg (orderPts $ contours V.! 0) img
        B.writeFile ("../data/Output/"++fnameNoExt++"_"++show iter++".tif") $ exceptError $ imencode OutputTiff correctedImg
        putStrLn $ "Wrote to file: ../data/Output/"++fnameNoExt++"_"++show iter++".tif" 
        when (V.length contours > 1) $ saveDetectedObjects (iter+1) (V.tail contours) img fname
    where fnameNoExt = reverse $ drop 4 $ reverse fname