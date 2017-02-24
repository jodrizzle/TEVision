module Transforms(
     cropImg
    ,perspectiveTransform
    ,threshBinary
) where
     
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as M
import GHC.Int (Int32)
import GHC.Word
import Linear.V2

cropImg:: M.Mat (CV.S '[height, width]) channels depth -> (V2 Int32) -> (V2 Int32) -> (M.Mat (CV.S '[CV.D, CV.D]) channels depth)
cropImg img topleft sz=  CV.exceptError $ CV.matSubRect img $ subRect topleft sz

perspectiveTransform:: M.Mat ('CV.S '[height, width]) channels depth -> M.Mat (M.ShapeT '[3,3]) (CV.S 1) (CV.S Double)-> M.Mat ('CV.S '[height, width]) channels depth
perspectiveTransform img t  = CV.exceptError $ CV.warpPerspective img t CV.InterNearest False True CV.BorderReplicate  

threshBinary::(depth `CV.In` '[Word8, Float])=> M.Mat (CV.S '[CV.D, CV.D]) ('CV.S 1) ('CV.S depth)->M.Mat (CV.S '[CV.D, CV.D]) ('CV.S 1) ('CV.S depth)
threshBinary img = fst $ CV.exceptError $ CV.threshold (CV.ThreshVal_Otsu) (CV.Thresh_Binary 255) img

subRect:: (V2 Int32)->(V2 Int32)->CV.Rect Int32
subRect topleft sz = CV.toRect $ CV.HRect topleft sz  --topleft (dist from left, dist from top), rect-size (width, height)