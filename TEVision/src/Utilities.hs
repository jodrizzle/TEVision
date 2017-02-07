module Utilities (
    getContours
   ,showImage
)   where

import Control.Monad (void)
import Control.Monad.Primitive
import GHC.Int (Int32)
import GHC.Word 
import System.Environment 
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified OpenCV.ImgProc.StructuralAnalysis as SA 
import qualified Data.Vector as V

showImage::String-> M.Mat (CV.S '[height, width]) channels depth-> IO ()
showImage title img = CV.withWindow title $ \window -> do  --display image
                        CV.imshow window img
                        void $ CV.waitKey 100000
                        
getContours :: PrimMonad m => M.Mat ('CV.S '[h0, w0]) ('CV.S 1) ('CV.S Word8) -> m (V.Vector SA.Contour)
getContours image = do
                    imageM <- CV.thaw image
                    contours_vector <- CV.findContours SA.ContourRetrievalList SA.ContourApproximationSimple imageM
                    pure contours_vector
        
perspectiveTransform:: M.Mat ('CV.S '[height, width]) channels depth -> M.Mat (M.ShapeT '[3,3]) (CV.S 1) (CV.S Double)-> M.Mat ('CV.S '[height, width]) channels depth
perspectiveTransform img t  = CV.exceptError $ CV.warpPerspective img t CV.InterNearest False True CV.BorderReplicate