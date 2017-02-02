module Main where

import Lib
import Utils    
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
        imgGS <- CV.imdecode CV.ImreadGrayscale <$> B.readFile ("data/"++fname)
       -- imgG  <- CV.imdecode CV.ImreadUnchanged <$> B.readFile ("data/"++fname)
      --  hmmm  <- CV.imdecode CV.ImreadUnchanged <$> B.readFile ("data/"++fname)
        --putStrLn . show $ :t img
       -- CV.withWindow "testOriginal" $ \window -> do
         --       CV.imshow window imgG
        --        void $ CV.waitKey 100000
        --CV.withWindow "testGrayscale" $ \window -> do
        --        CV.imshow window imgGS
        --        void $ CV.waitKey 100000
        let cannyImg = CV.exceptError $ CV.canny 30 200 Nothing CV.CannyNormL1 imgGS  
        CV.withWindow "testCanny" $ \window -> do
                CV.imshow window cannyImg
                void $ CV.waitKey 100000     
               -- imgG <- cvtColor bgr gray circles_1000x625
               
--cannyImg:: forall shape channels depth (CV.Mat shape channels depth ~ Lambda)=> CV.Mat shape (1) depth
--cannyImg img= CV.exceptError $ CV.canny 30 200 Nothing CV.CannyNormL1 img
