module ImageIO(
    showImage
   ,showDetectedObjects
)   where
    
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified OpenCV.ImgProc.StructuralAnalysis as SA 
import qualified Data.Vector as V
import Control.Monad (void)
import GHC.Int (Int32)
import Linear.V2

import Transforms
import Utilities

showImage::String-> M.Mat (CV.S '[height, width]) channels depth-> IO ()
showImage title img = CV.withWindow title $ \window -> do  --display image
                        CV.imshow window img
                        CV.resizeWindow window 500 500
                        void $ CV.waitKey 100000
                        
showDetectedObjects::Int->(V.Vector SA.Contour)->M.Mat (CV.S '[height, width]) channels depth->IO ()
showDetectedObjects iter contours imgOrig
    | (V.null contours)   == True = error "NO OBJECTS DETECTED!"
    | (V.length contours) == 1    = showImage ("Object "++(show iter)) (cropImg imgOrig (CV.fromPoint (CV.rectTopLeft uprightBounder)::(V2 Int32)) (CV.fromSize  (CV.rectSize uprightBounder)::(V2 Int32)))
    | otherwise                   = do
                                    showImage ("Object "++(show iter)) (cropImg imgOrig (CV.fromPoint (CV.rectTopLeft uprightBounder)::(V2 Int32)) (CV.fromSize  (CV.rectSize uprightBounder)::(V2 Int32)))
                                    showDetectedObjects (iter+1) (V.tail contours) imgOrig
    where uprightBounder = getUprightBoundRect contours
