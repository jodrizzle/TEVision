module Main where

--import Lib
--import Utils    
import Control.Monad ( void )
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified Data.ByteString as B 
import System.Environment    

--myImg = CV.imdecode CV.ImreadUnchanged <$> B.readFile ("data/box.png")
main :: IO ()
main = do
        args<-getArgs
        let fname= args !! 0
        imgOrig  <- CV.imdecode CV.ImreadUnchanged <$> B.readFile ("data/"++fname)
        imgGS    <- CV.imdecode CV.ImreadGrayscale <$> B.readFile ("data/"++fname)
        --convert imgGS (CV.D) to (CV.S Word8) to ppass to imshow 
 
        CV.withWindow "Original" $ \window -> do
                CV.imshow window imgOrig
                void $ CV.waitKey 100000
        CV.withWindow "Grayscale" $ \window -> do
                CV.imshow window imgGS
                void $ CV.waitKey 100000
        --let cannyImg = CV.exceptError $ CV.canny 30 200 Nothing CV.CannyNormL1 imgGS  
        CV.withWindow "testCanny" $ \window -> do
                --unwrapped <- cannyImg
                --CV.imshow window $ cannyImg imgGS
                void $ CV.waitKey 100000     
           --Expected type: 
           --M.Mat ('CV.S '['CV.D, 'CV.D]) 'CV.D ('CV.S GHC.Word.Word8)
           -- Actual type: 
           --M.Mat ('CV.S '['CV.D, 'CV.D]) 'CV.D 'CV.D    
--CANNY takes:
--Mat (S '[h, w]) channels (S Word8)           
           
                    
--cannyImg:: forall shape channels depth (CV.Mat shape channels depth ~ Lambda)=> CV.Mat shape (1) depth
cannyImg img= CV.exceptError $ CV.canny 30 200 Nothing CV.CannyNormL1 img
