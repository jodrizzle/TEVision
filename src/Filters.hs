module Filters(
    cannyImg
   ,gaussianBlurImg
   ,enhanceEdges 
   ,morphImg   
)   where
    
import Data.Proxy
import GHC.Int (Int16,Int32)
import GHC.Word  
import Linear.V2  
import OpenCV

cannyImg::Mat (S [h, w]) channels (S Word8)->(Mat (S [h, w]) (S 1) (S Word8))  
cannyImg img = exceptError $ canny 25 200 Nothing CannyNormL1 img

gaussianBlurImg:: (depth `In` [Word8, Word16, Float, Double], channels `In` [1, 3, 4])  => (Mat shape (S channels) (S depth))->Int32-> Mat shape (S channels) (S depth)
gaussianBlurImg imgO size = exceptError $ gaussianBlur (V2 size size ::V2 Int32) 0 0 imgO 

enhanceEdges::(depth `In` [Word8, Word16, Int16, Float, Double]) => Mat shape channels (S depth)-> IO (Mat shape channels (S depth))
enhanceEdges img= do
    kern <- newMatx33d (-1) (-1) (-1) (-1) (9) (-1) (-1) (-1) (-1)
    let kernelMat = exceptError $ coerceMat (toMat kern)::Mat (S [ S 3, S 3]) (S 1) (S Double) 
    return (exceptError $ filter2D img (kernelMat) (Nothing :: Maybe Point2i) 0 BorderReplicate) 

morphImg:: (depth `In` [Word8, Word16, Int16, Float, Double]) => MorphOperation->Int->Mat shape channels (S depth)->IO (Mat shape channels (S depth))
morphImg op rep img = do
    kern <- newMatx33d (-1) (-1) (-1) (-1) (9) (-1) (-1) (-1) (-1)
    let kernelMat = exceptError $ coerceMat (toMat kern)::Mat D D D --(S [ S 3, S 3]) (S 1) (S Double) 
    let kernelCross = exceptError $ coerceMat (getStructElem $ MorphCross $ toPoint (pure (-1) :: V2 Int32))::Mat D D D
    return (exceptError $ morphologyEx img op kernelCross (Nothing::Maybe Point2i) rep BorderReplicate)
    
getStructElem::MorphShape->Mat (ShapeT [20, 20]) (S 1) (S Word8)
getStructElem shape = exceptError $ do
    mat <- getStructuringElement shape (Proxy :: Proxy 20) (Proxy :: Proxy 20)
    img <- matConvertTo (Just 255) Nothing mat
    bitwiseNot img