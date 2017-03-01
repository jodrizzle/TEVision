module Transforms(
     cropImg
    ,perspectiveTransform
    ,threshBinary
) where
     
import OpenCV
import Data.Proxy
import GHC.Int (Int32)
import GHC.Word
import Linear.V2

cropImg:: Mat (S [height, width]) channels depth -> (V2 Int32) -> (V2 Int32) -> (Mat (S [D, D]) channels depth)
cropImg img topleft sz=  exceptError $ matSubRect img $ subRect topleft sz

perspectiveTransform:: Mat (S [height, width]) channels depth -> Mat (ShapeT [3,3]) (S 1) (S Double)-> Mat (S [height, width]) channels depth
perspectiveTransform img t  = exceptError $ warpPerspective img t InterNearest False True BorderReplicate  

threshBinary::(depth `In` [Word8, Float])=> Mat (S [D, D]) (S 1) (S depth)->Mat (S [D, D]) (S 1) (S depth)
threshBinary img = fst $ exceptError $ threshold (ThreshVal_Otsu) (Thresh_Binary 255) img

subRect:: (V2 Int32)->(V2 Int32)->Rect Int32
subRect topleft sz = toRect $ HRect topleft sz  --topleft (dist from left, dist from top), rect-size (width, height)