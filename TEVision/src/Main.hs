module Main where
 
import Control.Monad ( void )
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified Data.ByteString as B 
import System.Environment 
import GHC.Word   

main :: IO ()
main = do
        args<-getArgs
        let fname = args !! 0
        imgOrig  <- CV.imdecode CV.ImreadUnchanged <$> B.readFile ("data/"++fname)      
        imgGS    <- CV.imdecode CV.ImreadGrayscale <$> B.readFile ("data/"++fname)
        
       -- blurred <- medianBlurImg (M.unsafeCoerceMat imgOrig :: M.Mat ('CV.S '[ 'CV.D, 'CV.D]) ('CV.S 3) ('CV.S Word8))
        CV.withWindow "Original" $ \window -> do  --display original image
                CV.imshow window imgOrig
                void $ CV.waitKey 100000
        CV.withWindow "Grayscale" $ \window -> do --display image in grayscale
                CV.imshow window imgGS  
                void $ CV.waitKey 100000
         
         
  
        --let changedMat = CV.exceptError $ M.coerceMat imgGS  --returns -> CvExcept (Mat shapeOut channelsOut depthOut)
        CV.withWindow "testCanny" $ \window -> do --display canny edge detected image                
         --       CV.imshow window blurred --cannyImg imgFormatted  --cannyImg requires Mat (S '[h, w]) channels (S Word8)
                CV.imshow window $ cannyImg (M.unsafeCoerceMat imgOrig :: M.Mat ('CV.S '[ 'CV.D, 'CV.D]) ('CV.S 3) ('CV.S Word8))
                void $ CV.waitKey 100000     
                 {-imshow:: Window	 
                           -> Mat (S '[height, width]) channels depth	 
                           -> IO ()	 
                 -}
                                  
--      cannyImg:: forall shape channels depth. (M.Mat shape channels depth ~ Img)=> M.Mat shape ('CV.S 1) depth
cannyImg img = CV.exceptError $ CV.canny 30 200 Nothing CV.CannyNormL1 img
{-
medianBlurImg:: (depth `CV.In` '[Word8, Word16, Float], channels `CV.In` '[1, 3, 4]) => (M.Mat shape ('CV.S channels) ('CV.S depth))-> CV.CvExcept(M.Mat shape ('CV.S channels) ('CV.S depth)) 
medianBlurImg imgO = CV.medianBlur imgO 13
 
-}
--let     imgFormatted::(Double dstDepth0)=> CV.CvExcept (M.Mat a b dstDepth0)
        --        imgFormatted = formatShow imgOrig
        --let imgForm = CV.coerceMat imgOrig
        --convert imgGS (CV.D) to (CV.S Word8) to pass to imshow 
--formatShow img = CV.matConvertTo Nothing Nothing img  
 --toDepthDS :: a -> DS Depth
                 {-
                 matConvertTo:: ToDepthDS (Proxy dstDepth)	 
                                => Maybe Double	        Optional scale factor.
                                -> Maybe Double	        Optional delta added to the scaled values.
                                -> Mat shape channels srcDepth	 
                                -> CvExcept (Mat shape channels dstDepth)	 
                                Converts an array to another data type with optional scaling-}
           --Expected type: 
           --M.Mat ('CV.S '['CV.D, 'CV.D]) 'CV.D ('CV.S GHC.Word.Word8)
           --Actual type: 
           --M.Mat ('CV.S '['CV.D, 'CV.D]) 'CV.D 'CV.D         
--formatShow::{-(Fractional destDepth)=>-}(M.Mat shape0 channels0 depth0)->(CV.CvExcept (M.Mat shape channels depth))      
