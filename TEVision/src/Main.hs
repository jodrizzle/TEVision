module Main where
import Control.Monad ( void )
import qualified OpenCV as CV
import qualified Data.ByteString as B 
import System.Environment            
main :: IO ()
main = do
        args<-getArgs
        let fname= args !! 0
        imgGS <- CV.imdecode CV.ImreadGrayscale <$> B.readFile ("data/"++fname)
        imgG <- CV.imdecode CV.ImreadUnchanged <$> B.readFile ("data/"++fname)
        --putStrLn . show $ :t img
        CV.withWindow "testOriginal" $ \window -> do
                CV.imshow window imgG
                void $ CV.waitKey 100000
        CV.withWindow "testGrayscale" $ \window -> do
                CV.imshow window imgGS
                void $ CV.waitKey 100000
                
               -- imgG <- cvtColor bgr gray circles_1000x625
