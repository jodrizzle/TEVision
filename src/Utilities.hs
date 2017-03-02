module Utilities (  
    approximateContours
   ,rawContours
   ,getContours
   ,getPt 
   ,sideLengths
   ,isLong
   ,isQuad
   ,cvtPointToCFloat
   ,orderPts
   ,isImgFile
   ,isSimplePolygon
   ,green
   ,red
)   where

import qualified Data.Vector as V

import Control.Monad.Primitive
import Data.Function
import Data.List
import Foreign.C.Types
import GHC.Int (Int32)
import GHC.Word 
import Linear
import OpenCV

green, red   :: Scalar
green       = toScalar (V4   0 255   0 255 :: V4 Double)
red         = toScalar (V4   0   0 255 255 :: V4 Double)

-- returns Vector : [TL,TR,BL,BR]
orderPts::V.Vector Point2i->V.Vector Point2i
orderPts pts = V.fromList [getVertex take take pts, getVertex drop take pts, getVertex take drop pts, getVertex drop drop pts]

--use this to get a vertex in TL, TR, BL, BR
--top: fn2 is 'take'; bottom: fn2 is 'drop'; right: fn1 is 'drop'; left: fn1 is 'take'  
getVertex::(Int -> [Point2i] -> [Point2i])->(Int -> [Point2i] -> [Point2i])->V.Vector Point2i->Point2i 
getVertex fn1 fn2 = head . fn1 1 . sortBy (compare `on` (getXComp . fromPoint)) . fn2 2 . sortBy (compare `on` (getYComp . fromPoint)) . V.toList

getPt::Int->V.Vector Point2i->V2 Int32
getPt num a = fromPoint $ a V.! num

cvtPointToCFloat::V2 Int32->V2 CFloat
cvtPointToCFloat (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

getXComp::V2 Int32->Int32
getXComp (V2 x _) = x
getYComp::V2 Int32->Int32
getYComp (V2 _ y) = y

getContours :: PrimMonad m => Mat (S [h0, w0]) (S 1) (S Word8) -> m (V.Vector Contour)
getContours image = do
                    imageM <- thaw image
                    contours_vector <- findContours ContourRetrievalExternal ContourApproximationSimple imageM
                    pure contours_vector

approximateContours::PrimMonad m =>V.Vector Contour->m (V.Vector (V.Vector Point2i)) 
approximateContours conts
    | V.length conts == 1 = do
            cont <- approxPolyDP (contourPoints $ V.head conts) (0.1*peri) True
            return (V.singleton cont)
    | otherwise           = do
            cont <- approxPolyDP (contourPoints $ V.head conts) (0.1*peri) True
            remainder <- pure (V.tail conts) >>= approximateContours
            let conc = (V.singleton cont) V.++ remainder
            return conc
    where peri = exceptError $ arcLength (contourPoints (V.head conts)) True    
          
rawContours::PrimMonad m =>V.Vector Contour->m (V.Vector (V.Vector Point2i)) 
rawContours conts
    | V.length conts == 1 = do
            let cont = (contourPoints $ V.head conts)
            return (V.singleton cont)
    | otherwise           = do
            let cont = (contourPoints $ V.head conts)
            remainder <- pure (V.tail conts) >>= rawContours
            let conc = (V.singleton cont) V.++ remainder
            return conc             
  
isQuad::V.Vector Point2i->Bool
isQuad pts = (V.length pts == 4)

isLong::V.Vector Point2i->Bool
isLong pts = (exceptError $ arcLength pts True)>=1500

isImgFile::FilePath->Bool
isImgFile nm = (reverse $ take 3 $ reverse nm) `elem` ["jpg","bmp","peg","png", "gif","tif","iff"]

isSimplePolygon::V.Vector Point2i->Bool  --check that shortest side is at least half of second shortest side
isSimplePolygon v = (sides !! 0)>=(0.5*(sides !! 1))  
    where sides = (sort $ sideLengths v)

sideLengths::V.Vector Point2i->[CFloat]
sideLengths v = [fromIntegral $ round $ exceptError $ arcLength (V.fromList [(v V.! 0) , (v V.! 1)]) False  
                ,fromIntegral $ round $ exceptError $ arcLength (V.fromList [(v V.! 0) , (v V.! 2)]) False
                ,fromIntegral $ round $ exceptError $ arcLength (V.fromList [(v V.! 3) , (v V.! 1)]) False
                ,fromIntegral $ round $ exceptError $ arcLength (V.fromList [(v V.! 3) , (v V.! 2)]) False]