module Filters(
    cannyImg
   ,gaussianBlurImg
   ,getKernel
   ,laplacianFilter
   ,medianBlurImg
   ,medianFilter1D
   ,medianFilter1DFine

)   where
    
import qualified OpenCV as CV
import qualified OpenCV.Internal.Core.Types.Mat as M
import Data.List
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

     
medianFilter1D::[Int32]->Int->[Int32] --  start index at 4
medianFilter1D diffs ix 
    | (length diffs < 9)                = diffs 
    | (ix>((length diffs - 1)-4))       = diffs
    | otherwise                         = prePart++[medianWndw]++postPart
    where prePart    = (take ix diffs)
          medianWndw = sort ([diffs !! (ix-4)]++[diffs !! (ix-3)]++[diffs !! (ix-2)] ++ [diffs !! (ix-1)] ++ [diffs !! ix] ++ [diffs !! (ix+1)] ++ [diffs !! (ix+2)]++[diffs !! (ix+3)] ++[diffs !! (ix+4)]) !! 4
          postPart   = drop (ix+1) (medianFilter1D diffs (ix+1))    

medianFilter1DFine::[Int32]->Int->[Int32] --  start index at 1
medianFilter1DFine diffs ix 
    | (length diffs < 3)                = diffs 
    | (ix>((length diffs - 1)-1))       = diffs
    | otherwise                         = prePart++[medianWndw]++postPart
    where prePart    = (take ix diffs)
          medianWndw = sort ([diffs !! (ix-1)] ++ [diffs !! ix] ++ [diffs !! (ix+1)]) !! 1
          postPart   = drop (ix+1) (medianFilter1DFine diffs (ix+1))