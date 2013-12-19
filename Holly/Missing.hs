{-# OPTIONS_GHC -fno-warn-orphans #-}
module Holly.Missing
    ( createBuffer
    , getWindowOpacity
    ) where

import Control.Applicative ( (<$>) )
import Data.Default
import Foreign.C.Types

import Graphics.X11
import Graphics.X11.Xlib.Extras

import Holly.Types
import Holly.X11

getWindowOpacity :: Display -> Window -> IO Double
getWindowOpacity dpy win = do
    opacityAtom <- internAtom dpy "_NET_WM_WINDOW_OPACITY" False
    mOpacity <- getWindowProperty8 dpy opacityAtom win
    case mOpacity of
        Just [opacity] -> return $! fromIntegral opacity / maxOpacity
        _ -> return 1.0
  where
    maxOpacity :: Double
    maxOpacity = fromIntegral 0xff

createBuffer :: Display -> Drawable -> (Dimension, Dimension) -> CInt
             -> PictFormat -> IO Picture
createBuffer dpy draw (w, h) depth format = do
    pixmap <- createPixmap dpy draw w h depth
    picture <- createPicture dpy pixmap format 
        $ def { attr_repeat = Just repeatNormal }
    freePixmap dpy pixmap
    return picture
