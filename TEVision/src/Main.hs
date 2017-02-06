module Main where

import qualified Data.ByteString as B 
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified OpenCV.ImgProc.StructuralAnalysis as SA 
import qualified Data.Vector as V
--import ApproxPolyDP
import Control.Monad ( void )
import Control.Monad.Primitive
--import ConvexHull
import GHC.Int 
import GHC.Word  
import Linear.V2
--import RamerDouglasPeuckerParts
--import RightReceipt 
import System.Environment 
--  import Utils

main :: IO ()
main = do
        --Parse arguments
        args<-getArgs
        let fname = args !! 0 --filename is first argument
        let blurRegion = args !! 1 --blur kernel size is second argument
        --Read image
        imgOrig  <- CV.imdecode CV.ImreadUnchanged <$> B.readFile ("../data/"++fname)      
        imgGS    <- CV.imdecode CV.ImreadGrayscale <$> B.readFile ("../data/"++fname)
        --tighten matrix type constraints to work with canny and blur
        let formImg = M.unsafeCoerceMat imgGS :: M.Mat ('CV.S '[ 'CV.D, 'CV.D]) ('CV.S 3) ('CV.S Word8)
        let kernel = getKernel blurRegion
        let canniedImg = cannyImg (gaussianBlurImg formImg kernel)
        contours <- (findingContours canniedImg)
        putStrLn (show contours)
        imgMut <-  CV.thaw imgOrig
        --contourImg <- CV.drawContours (V.map SA.contourPoints contours) (CV.toScalar (0::Double,255::Double,0::Double)) (CV.OutlineContour CV.LineType_AA 1) imgMut
        
      --  let listOfPoints   = getListOfPoints (V.toList contours)
      --  let listOfAreas    = dictAreaPoints listOfPoints
      --  let screenContours = findReceipt listOfAreas

      --  putStrLn (show screenContours)
        
        --display results
        --showImage "Contours" contourImg
        showImage "Original" imgOrig
        showImage "Grayscale" imgGS
        showImage "Edges" $ cannyImg formImg
        showImage "Edges (median blur)" $ cannyImg (medianBlurImg formImg kernel)
        showImage "Edges (Gaussian blur)" canniedImg
      

{-
drawContours:: (ToScalar color, PrimMonad m)=> Vector (Vector Point2i)-> color-> ContourDrawMode-> Mut (Mat (S '[h, w]) channels depth) (PrimState m) -> m ()    
drawContours (V.map contourPoints contours)
                        ())
                        (OutlineContour LineType_AA 1)
                        imgM
   
   -}      

      
      
      
getKernel::String->Int32
getKernel reg = read reg

showImage::String-> M.Mat (CV.S '[height, width]) channels depth-> IO ()
showImage title img = CV.withWindow title $ \window -> do  --display image
                        CV.imshow window img
                        void $ CV.waitKey 100000
                        
cannyImg::M.Mat (CV.S '[h, w]) channels (CV.S Word8)->(M.Mat (CV.S '[h, w]) (CV.S 1) (CV.S Word8))  
cannyImg img = CV.exceptError $ CV.canny 75 200 Nothing CV.CannyNormL2 img

medianBlurImg:: (depth `CV.In` '[Word8, Word16, Float], channels `CV.In` '[1, 3, 4]) => (M.Mat shape ('CV.S channels) ('CV.S depth))->Int32-> M.Mat shape ('CV.S channels) ('CV.S depth)
medianBlurImg imgO regionSize = CV.exceptError $ CV.medianBlur imgO regionSize

gaussianBlurImg:: (depth `CV.In` '[Word8, Word16, Float, Double], channels `CV.In` '[1, 3, 4])  => (M.Mat shape ('CV.S channels) ('CV.S depth))->Int32-> M.Mat shape ('CV.S channels) ('CV.S depth)
gaussianBlurImg imgO size = CV.exceptError $ CV.gaussianBlur (V2 size size ::V2 Int32) 0 0 imgO 
            
findingContours :: PrimMonad m => M.Mat ('CV.S '[h0, w0]) ('CV.S 1) ('CV.S Word8) -> m (V.Vector SA.Contour)
findingContours image = do
                    imageM <- CV.thaw image
                    contours_vector <- CV.findContours SA.ContourRetrievalList SA.ContourApproximationSimple imageM
                    pure contours_vector