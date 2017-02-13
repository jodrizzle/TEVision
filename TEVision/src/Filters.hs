module Filters(
    cannyImg
   ,gaussianBlurImg
   ,getKernel
   ,laplacianFilter
   ,medianBlurImg

)   where
    
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as M
import GHC.Int (Int32)
import GHC.Word  
import Linear.V2   

cannyImg::M.Mat (CV.S '[h, w]) channels (CV.S Word8)->(M.Mat (CV.S '[h, w]) (CV.S 1) (CV.S Word8))  
cannyImg img = CV.exceptError $ CV.canny 25 200 Nothing CV.CannyNormL2 img

gaussianBlurImg:: (depth `CV.In` '[Word8, Word16, Float, Double], channels `CV.In` '[1, 3, 4])  => (M.Mat shape ('CV.S channels) ('CV.S depth))->Int32-> M.Mat shape ('CV.S channels) ('CV.S depth)
gaussianBlurImg imgO size = CV.exceptError $ CV.gaussianBlur (V2 size size ::V2 Int32) 0 0 imgO 

getKernel::String->Int32
getKernel reg = read reg

--has to be grayscale input                    
laplacianFilter::M.Mat shape ('CV.S 1) ('CV.S Word8)-> M.Mat shape ('CV.S 1) ('CV.S Word8)
laplacianFilter img = CV.exceptError $ CV.laplacian Nothing Nothing Nothing Nothing img

medianBlurImg:: (depth `CV.In` '[Word8, Word16, Float], channels `CV.In` '[1, 3, 4]) => (M.Mat shape ('CV.S channels) ('CV.S depth))->Int32-> M.Mat shape ('CV.S channels) ('CV.S depth)
medianBlurImg imgO regionSize = CV.exceptError $ CV.medianBlur imgO regionSize