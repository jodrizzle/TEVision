module Main where
 
import Control.Monad ( void )
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified Data.ByteString as B 
import System.Environment 
import GHC.Word  
import GHC.Int 
import Linear.V2

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
        showImage "Original" imgOrig
        showImage "Grayscale" imgGS
        showImage "Edges" $ cannyImg formImg
        showImage "Edges (median blur)" $ cannyImg (medianBlurImg formImg kernel)
        showImage "Edges (Gaussian blur)" $  cannyImg (gaussianBlurImg formImg kernel)
                                  
getKernel::String->Int32
getKernel reg = read reg

showImage::String-> M.Mat (CV.S '[height, width]) channels depth-> IO ()
showImage title img = CV.withWindow title $ \window -> do  --display image
                        CV.imshow window img
                        void $ CV.waitKey 100000
                        
cannyImg::M.Mat (CV.S '[h, w]) channels (CV.S Word8)->(M.Mat (CV.S '[h, w]) (CV.S 1) (CV.S Word8))  
cannyImg img = CV.exceptError $ CV.canny 30 200 Nothing CV.CannyNormL1 img

medianBlurImg:: (depth `CV.In` '[Word8, Word16, Float], channels `CV.In` '[1, 3, 4]) => (M.Mat shape ('CV.S channels) ('CV.S depth))->Int32-> M.Mat shape ('CV.S channels) ('CV.S depth)
medianBlurImg imgO regionSize = CV.exceptError $ CV.medianBlur imgO regionSize

gaussianBlurImg:: (depth `CV.In` '[Word8, Word16, Float, Double], channels `CV.In` '[1, 3, 4])  => (M.Mat shape ('CV.S channels) ('CV.S depth))->Int32-> M.Mat shape ('CV.S channels) ('CV.S depth)
gaussianBlurImg imgO size = CV.exceptError $ CV.gaussianBlur (V2 size size ::V2 Int32) 0 0 imgO 
            
