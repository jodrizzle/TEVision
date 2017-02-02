{-# LANGUAGE TypeFamilies #-}

module Lib
    ( controller
    ) where

import BlurImage
import ResizeImage
import Utils
import Control.Monad ( void )
import Data.Word
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified OpenCV as CV
import qualified Data.ByteString as B

controller :: IO (CV.Mat (CV.S '[CV.D, CV.D]) (CV.S 1) (CV.S Word8))
controller = do
    file <- B.readFile "path/to/image.jpg"
    img <- return $ CV.imdecode CV.ImreadGrayscale file
    resized_little_img <- resizeImage img --little image for making a blur in and find the receipt
    blurImage ((CV.exceptError $ M.coerceMat resized_little_img) :: M.Mat (CV.S '[ CV.D, CV.D]) (CV.S 1) (CV.S Word8))
