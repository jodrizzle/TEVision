module Filters(
    cannyImg
   ,gaussianBlurImg
   ,medianBlurImg
   ,boxBlurImg
   ,enhanceEdges
   ,dilateImg
   ,erodeImg    
   ,openingImg
   ,closingImg
   ,morphImg
)   where
    
import OpenCV
import Data.List
import Data.Proxy
import GHC.Int (Int16,Int32)
import GHC.Word  
import Linear.V2  
import System.IO.Unsafe ( unsafePerformIO )

import Utilities

cannyImg::Mat (S [h, w]) channels (S Word8)->(Mat (S [h, w]) (S 1) (S Word8))  
cannyImg img = exceptError $ canny 25 200 Nothing CannyNormL1 img

gaussianBlurImg:: (depth `In` [Word8, Word16, Float, Double], channels `In` [1, 3, 4])  => (Mat shape (S channels) (S depth))->Int32-> Mat shape (S channels) (S depth)
gaussianBlurImg imgO size = exceptError $ gaussianBlur (V2 size size ::V2 Int32) 0 0 imgO 

boxBlurImg:: (depth `In` [Word8, Word16, Int16, Float, Double], channels `In` [1, 3, 4])  => (Mat shape (S channels) (S depth))->Int32-> Mat shape (S channels) (S depth)
boxBlurImg imgO size = exceptError $ blur (V2 size size ::V2 Int32) imgO

medianBlurImg:: (depth `In` [Word8, Word16, Float], channels `In` [1, 3, 4]) => (Mat shape (S channels) (S depth))->Int32-> Mat shape (S channels) (S depth)
medianBlurImg imgO regionSize = exceptError $ medianBlur imgO regionSize

enhanceEdges::(depth `In` [Word8, Word16, Int16, Float, Double]) => Mat shape channels (S depth)-> IO (Mat shape channels (S depth))
enhanceEdges img= do
    kern <- newMatx33d (-1) (-1) (-1) (-1) (9) (-1) (-1) (-1) (-1)
    let kernelMat = exceptError $ coerceMat (toMat kern)::Mat (S [ S 3, S 3]) (S 1) (S Double) 
    return (exceptError $ filter2D img (kernelMat) (Nothing :: Maybe Point2i) 0 BorderReplicate) 
   
erodeImg:: (depth `In` [Word8, Word16, Int16, Float, Double])=> Int->Mat shape channels (S depth)->Mat shape channels (S depth)
erodeImg rough img = exceptError $ erode img Nothing (Nothing::Maybe Point2i) rough BorderReplicate

dilateImg:: (depth `In` [Word8, Word16, Int16, Float, Double])=> Int->Mat shape channels (S depth)->Mat shape channels (S depth)
dilateImg rough img = exceptError $ dilate img Nothing (Nothing::Maybe Point2i) rough BorderReplicate

openingImg:: (depth `In` [Word8, Word16, Int16, Float, Double])=> Mat shape channels (S depth)->Mat shape channels (S depth)
openingImg = (dilateImg 5) . (erodeImg 5)

closingImg:: (depth `In` [Word8, Word16, Int16, Float, Double])=> Mat shape channels (S depth)->Mat shape channels (S depth)
closingImg = (erodeImg 5) . (dilateImg 5)

morphImg:: (depth `In` [Word8, Word16, Int16, Float, Double]) => Mat shape channels (S depth)->MorphOperation->Int->IO (Mat shape channels (S depth))
morphImg img op rep = do
    kern <- newMatx33d (-1) (-1) (-1) (-1) (9) (-1) (-1) (-1) (-1)
    let kernelMat = exceptError $ coerceMat (toMat kern)::Mat D D D --(S [ S 3, S 3]) (S 1) (S Double) 
    return (exceptError $ morphologyEx img op kernelMat (Nothing::Maybe Point2i) rep BorderReplicate)