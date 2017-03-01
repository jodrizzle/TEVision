module Utilities (  
    findEnclosingRectangle
   ,approximateContours
   ,rawContours
   ,getContours
   ,getPt 
   ,getUprightBoundRect
   ,getXComp
   ,getYComp
   ,isLong
   ,isQuad
   ,makePoint2f
   ,orderPts
   ,findLargestContourIndex
   ,getAreas
   ,contFloat
   ,isImgFile
   ,printSideLengths
   ,isSimplePolygon
   ,transparent
   ,white
   ,black
   ,blue
   ,green
   ,red
)   where

import Control.Monad (void)
import Control.Monad.Primitive
import Data.Function
import Data.List
import Foreign.C.Types
import GHC.Int (Int32)
import GHC.Word 
import Linear
import System.Environment 
import OpenCV
import qualified Data.Vector as V

transparent, white, black, blue, green, red   :: Scalar
transparent = toScalar (V4 255 255 255   0 :: V4 Double)
white       = toScalar (V4 255 255 255 255 :: V4 Double)
black       = toScalar (V4   0   0   0 255 :: V4 Double)
blue        = toScalar (V4 255   0   0 255 :: V4 Double)
green       = toScalar (V4   0 255   0 255 :: V4 Double)
red         = toScalar (V4   0   0 255 255 :: V4 Double)

orderPts::V.Vector Point2i->V.Vector Point2i
orderPts pts = V.fromList [topLeft pts, topRight pts, bottomLeft pts, bottomRight pts]

topLeft::V.Vector Point2i->Point2i
topLeft     = head . take 1 . sortBy (compare `on` (getXComp . fromPoint)) . take 2 . sortBy (compare `on` (getYComp . fromPoint)) . V.toList
topRight::V.Vector Point2i->Point2i
topRight    = head . drop 1 . sortBy (compare `on` (getXComp . fromPoint)) . take 2 . sortBy (compare `on` (getYComp . fromPoint)) . V.toList
bottomLeft::V.Vector Point2i->Point2i
bottomLeft  = head . take 1 . sortBy (compare `on` (getXComp . fromPoint)) . drop 2 . sortBy (compare `on` (getYComp . fromPoint)) . V.toList
bottomRight::V.Vector Point2i->Point2i
bottomRight = head . drop 1 . sortBy (compare `on` (getXComp . fromPoint)) . drop 2 . sortBy (compare `on` (getYComp . fromPoint)) . V.toList

getPt::Int->V.Vector Point2i->V2 Int32
getPt num a = fromPoint $ a V.! (num-1)

makePoint2f::V2 Int32->V2 CFloat
makePoint2f (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

getXComp::V2 Int32->Int32
getXComp (V2 x _) = x
getYComp::V2 Int32->Int32
getYComp (V2 _ y) = y

findEnclosingRectangle:: IsPoint2 point2 Int32 => V.Vector (point2 Int32) -> RotatedRect
findEnclosingRectangle = minAreaRect
    
getUprightBoundRect::   V.Vector Point2i-> Rect2i 
getUprightBoundRect contour= rotatedRectBoundingRect $ (findEnclosingRectangle contour)  --rect2i  

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
          
findLargestContourIndex::V.Vector Double->Int
findLargestContourIndex areas 
    | V.length areas == 0 = (-1)
    | otherwise           = snd . maximum $ zip (V.toList areas) [0..]

getAreas::V.Vector (V.Vector Point2i)->V.Vector Double
getAreas conts
    | V.length conts == 0 =  V.empty
    | V.length conts == 1 =  V.singleton $ exceptError $ contourArea (contFloat (V.head conts)) ContourAreaAbsoluteValue
    | otherwise           = (V.singleton (exceptError (contourArea (contFloat (V.head conts)) ContourAreaAbsoluteValue))) V.++ (getAreas (V.tail conts))

contFloat::V.Vector Point2i -> V.Vector Point2f
contFloat = V.map (toPoint . makePoint2f . fromPoint)                    
  
isQuad::V.Vector Point2i->Bool
isQuad pts = (V.length pts == 4)

isLong::V.Vector Point2i->Bool
isLong pts = (exceptError $ arcLength pts True)>=1500

isImgFile::FilePath->Bool
isImgFile nm = (reverse $ take 3 $ reverse nm) `elem` ["jpg","bmp","peg","png", "gif","tif","iff"]


printSideLengths::((V.Vector Point2i),Double)->IO () --check that shortest side is at least half of second shortest side
printSideLengths (v,area) = do
    putStrLn $ "Area:                :\t"++show area
    printSides v
    
isSimplePolygon::V.Vector Point2i->Bool
isSimplePolygon v = (fst $ shortestSides v)>=(0.5*(snd $ shortestSides v))  

shortestSides::V.Vector Point2i->(Double,Double) --shortest, second shortest
shortestSides v = (head lengths,head $ tail lengths)
    where lengths = sort [    
             exceptError $ arcLength (V.fromList [(v V.! 0) , (v V.! 1)]) False  
            ,exceptError $ arcLength (V.fromList [(v V.! 0) , (v V.! 2)]) False
            ,exceptError $ arcLength (V.fromList [(v V.! 3) , (v V.! 1)]) False
            ,exceptError $ arcLength (V.fromList [(v V.! 3) , (v V.! 2)]) False    ]

printSides::(V.Vector Point2i)->IO ()--
printSides v = do
        rs <-   V.sequence $ V.fromList [    
             putStrLn $ show $ exceptError $ arcLength (V.fromList [(v V.! 0) , (v V.! 1)]) False  
            ,putStrLn $ show $ exceptError $ arcLength (V.fromList [(v V.! 0) , (v V.! 2)]) False
            ,putStrLn $ show $ exceptError $ arcLength (V.fromList [(v V.! 3) , (v V.! 1)]) False
            ,putStrLn $ show $ exceptError $ arcLength (V.fromList [(v V.! 3) , (v V.! 2)]) False    ]
        putStrLn $ show rs