module Transforms(
    correctImg
   ,threshBinary
) where

import qualified Data.Vector as V
     
import OpenCV
import GHC.Int (Int32)
import GHC.Word
import Linear.V2

import Utilities

threshBinary::(depth `In` [Word8, Float])=> Mat (S [D, D]) (S 1) (S depth)->Mat (S [D, D]) (S 1) (S depth)
threshBinary img = fst $ exceptError $ threshold (ThreshVal_Otsu) (Thresh_Binary 255) img

correctImg::V.Vector Point2i->Mat (S [ D, D]) (D) (D)->Mat (S [ D, D]) (D) (D)
correctImg contour img = cropImg (perspectiveTransform img (getPerspectiveTransform (V.map (cvtPointToCFloat) srcVec) dstVec)) (V2 (round x_coord) (round y_coord))
    where 
        x_coord = (sideLengths contour) !! 0
        y_coord = (sideLengths contour) !! 1
        srcVec = V.fromList [getPt 0 contour, getPt 1       contour, getPt 2 contour, getPt 3       contour]
        dstVec = V.fromList [V2    0 0      , V2    x_coord 0      , V2    0 y_coord, V2    x_coord y_coord]    

cropImg:: Mat (S [height, width]) channels depth -> V2 Int32 -> (Mat (S [D, D]) channels depth)
cropImg img sz=  exceptError $ matSubRect img $ (toRect . HRect (V2 0 0)) sz 

perspectiveTransform:: Mat (S [height, width]) channels depth -> Mat (ShapeT [3,3]) (S 1) (S Double)-> Mat (S [height, width]) channels depth
perspectiveTransform img t  = exceptError $ warpPerspective img t InterNearest False True BorderReplicate  