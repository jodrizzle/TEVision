module Transforms(
     perspectiveTransform
    ,rotationMatrix
    ,warpAffineImg
    ,warpAffineInvImg
    ,subRect
    ,cropImg
    ,threshBinary
) where
     
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as M
import GHC.Int (Int32)
import GHC.Word
import Linear.V2

import Utilities

perspectiveTransform:: M.Mat ('CV.S '[height, width]) channels depth -> M.Mat (M.ShapeT '[3,3]) (CV.S 1) (CV.S Double)-> M.Mat ('CV.S '[height, width]) channels depth
perspectiveTransform img t  = CV.exceptError $ CV.warpPerspective img t CV.InterNearest False True CV.BorderReplicate

rotationMatrix :: M.Mat (M.ShapeT [2, 3]) ('CV.S 1) ('CV.S Double)
rotationMatrix = CV.getRotationMatrix2D (V2 256 170 ) 30 0.6

warpAffineImg :: M.Mat (CV.S '[height, width]) channels depth->(M.Mat (CV.S '[height, width]) channels depth)
warpAffineImg img = CV.exceptError $ CV.warpAffine img rotationMatrix CV.InterArea False False (CV.BorderConstant black)

warpAffineInvImg ::  M.Mat (CV.S '[height, width]) channels depth->(M.Mat (CV.S '[height, width]) channels depth)
warpAffineInvImg img= CV.exceptError $ CV.warpAffine img rotationMatrix CV.InterCubic True False (CV.BorderConstant black)

subRect:: (V2 Int32)->(V2 Int32)->CV.Rect Int32
subRect topleft sz = CV.toRect $ CV.HRect topleft sz  --topleft (dist from left, dist from top), rect-size (width, height)

cropImg:: M.Mat (CV.S '[height, width]) channels depth -> (V2 Int32) -> (V2 Int32) -> (M.Mat (CV.S '[CV.D, CV.D]) channels depth)
cropImg img topleft sz=  CV.exceptError $ CV.matSubRect img $ subRect topleft sz  

threshBinary::(depth `CV.In` '[Word8, Float])=> M.Mat (CV.S '[CV.D, CV.D]) ('CV.S 1) ('CV.S depth)->M.Mat (CV.S '[CV.D, CV.D]) ('CV.S 1) ('CV.S depth)
threshBinary img = fst $ CV.exceptError $ CV.threshold (CV.ThreshVal_Abs 150) (CV.Thresh_Binary 255) img