{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module ResizeImage
    ( 
    resizeImage
    ) where

import Utils
import Control.Monad ( void )
import Control.Monad.Except
import Data.Functor.Identity
import Data.Word
import Data.Proxy
import qualified OpenCV as CV
import Linear.V2
import OpenCV.TypeLevel
import qualified OpenCV.Internal.Core.Types.Mat as M
import qualified OpenCV.Core.Types.Size as S
import qualified OpenCV.ImgProc.GeometricImgTransform as GIT
import GHC.Int (Int32)

resizingImage :: (M.Mat (CV.S [CV.D, CV.D]) CV.D CV.D) -> CV.CvExcept (M.Mat (CV.S [CV.D, CV.D]) CV.D CV.D)
resizingImage image = GIT.resize (GIT.ResizeAbs $ S.toSize $ (getSize w h Nothing (Just 500))) CV.InterCubic image
    where
        [h, w] = getHandW image

resizeImage :: (M.Mat (S '[CV.D, CV.D]) CV.D CV.D) -> IO(M.Mat (CV.S [CV.D, CV.D]) CV.D CV.D)
resizeImage image = do        
    resized <- return $ resizingImage image
    return $ CV.exceptError $ resized
