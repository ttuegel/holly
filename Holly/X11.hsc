{-# LANGUAGE ForeignFunctionInterface #-}
module Holly.X11
    ( Damage
    , Located(..)
    , PictFormat
    , PictOp
    , Picture
    , PictureAttributes(..)
    , PolyEdge
    , PolyMode
    , Region
    , Repeat
    , composite
    , createBuffer
    , createPicture
    , createRegion
    , damageSubtract
    , destroyRegion
    , fillRectangle
    , findStandardFormat
    , findVisualFormat
    , getWindowOpacity
    , intersectRegion
    , repeatNormal
    , setPictureClipRegion
    , shapeRectangles
    , subwindowClipByChildren, subwindowIncludeInferiors
    , translateRegion
    , unionRegion
    , module Graphics.X11
    , module Graphics.X11.Xlib
    , module Graphics.X11.Xrender
    ) where

#include <X11/extensions/Xrender.h>

import Control.Monad (liftM)
import Data.Default
import Data.Foldable (for_)
import Data.Maybe (isJust)
import Foreign
import Foreign.C.Types

import Graphics.X11 hiding (Region)
import Graphics.X11.Xlib hiding (Region)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xrender

type Damage = XID
newtype PictFormat = PictFormat (Ptr PictFormat)
type PictOp = CInt
type Picture = XID
type PolyEdge = CInt
type PolyMode = CInt
type Region = XID
type Repeat = CInt

data PictureAttributes = PictureAttributes
    { attr_repeat :: Maybe Repeat
    , attr_subwindowMode :: Maybe CInt
    }

instance Default PictureAttributes where
    def = PictureAttributes
        { attr_repeat = Nothing, attr_subwindowMode = Nothing }

instance Storable PictureAttributes where
    sizeOf _ = #size XRenderPictureAttributes
    alignment _ = alignment (0 :: Word64)

    poke p attrs = do
        for_ (attr_repeat attrs) $ (#poke XRenderPictureAttributes, repeat) p
        for_ (attr_subwindowMode attrs) $ (#poke XRenderPictureAttributes, subwindow_mode) p

    peek p = do
        r <- (#peek XRenderPictureAttributes, repeat) p
        sw <- (#peek XRenderPictureAttributes, subwindow_mode) p
        return $! PictureAttributes
            { attr_repeat = Just r, attr_subwindowMode = Just sw }

subwindowIncludeInferiors, subwindowClipByChildren :: CInt
subwindowIncludeInferiors = #const IncludeInferiors
subwindowClipByChildren = #const ClipByChildren

repeatNormal :: CInt
repeatNormal = #const RepeatNormal

toMask :: PictureAttributes -> CULong
toMask attrs = foldr (.|.) 0
    $ map (\(x, f) -> if (f attrs) then x else 0)
    [ ((#const CPRepeat), isJust . attr_repeat)
    , ((#const CPSubwindowMode), isJust . attr_subwindowMode)
    ]

data Located a = Located
    { x, y :: CInt
    , val  :: a
    }

foreign import ccall "Xrender.h XRenderFindVisualFormat"
    findVisualFormat_ :: Ptr Display -> Visual -> IO (Ptr PictFormat)

findVisualFormat :: Display -> Visual -> IO PictFormat
findVisualFormat (Display pDpy) pVis =
    liftM PictFormat $ findVisualFormat_ pDpy pVis

foreign import ccall "Xrender.h XRenderFindStandardFormat"
    findStandardFormat_ :: Ptr Display -> CInt -> IO (Ptr PictFormat)

findStandardFormat :: Display -> CInt -> IO PictFormat
findStandardFormat (Display pDpy) fmt =
    liftM PictFormat $ findStandardFormat_ pDpy fmt

foreign import ccall "Xrender.h XRenderCreatePicture"
    createPicture_ :: Ptr Display -> Drawable -> Ptr PictFormat
                   -> CULong -> Ptr PictureAttributes -> IO Picture

createPicture :: Display -> Drawable -> PictFormat -> PictureAttributes
              -> IO Picture
createPicture (Display pDpy) draw (PictFormat pFmt) attrs =
    with attrs $ \pAttrs ->
    createPicture_ pDpy draw pFmt attrMask pAttrs
  where attrMask = toMask attrs

foreign import ccall "Xrender.h XRenderComposite"
    composite_ :: Ptr Display
               -> CInt              -- op
               -> Picture           -- src
               -> Picture           -- mask
               -> Picture           -- dst
               -> CInt -> CInt      -- src_x, src_y
               -> CInt -> CInt      -- mask_x, mask_y
               -> CInt -> CInt      -- dst_x, dst_y
               -> CUInt -> CUInt    -- width, height
               -> IO ()

composite :: Display -> PictOp -> Located Picture -> Maybe (Located Picture)
          -> Located Picture -> (CUInt, CUInt) -> IO ()
composite (Display pDpy) op src mMask dst (w, h) =
    composite_ pDpy (fromIntegral $ fromEnum op)
               (val src) mask (val dst)
               (x src) (y src)
               xMask yMask
               (x dst) (y dst)
               w h
  where (mask, xMask, yMask) = maybe (0, 0, 0) (\m -> (val m, x m, y m)) mMask

foreign import ccall "Xrender.h XRenderFillRectangle"
    fillRectangle_ :: Ptr Display
                   -> CInt                 -- op
                   -> Picture              -- dst
                   -> Ptr XRenderColor     -- color
                   -> CInt -> CInt         -- x, y
                   -> CUInt -> CUInt       -- w, h
                   -> IO ()

fillRectangle :: Display -> PictOp -> Picture -> XRenderColor
              -> CInt -> CInt -> CUInt -> CUInt -> IO ()
fillRectangle (Display pDpy) op dst color x y w h =
    with color $ \pColor ->
    fillRectangle_ pDpy (fromIntegral $ fromEnum op) dst pColor
                   x y w h

foreign import ccall "Xdamage.h XDamageSubtract"
    damageSubtract_ :: Ptr Display -> Damage -> Region -> Region -> IO ()

damageSubtract :: Display -> Damage -> Maybe Region -> Maybe Region -> IO ()
damageSubtract (Display pDpy) dam mRepair mParts =
    damageSubtract_ pDpy dam repair parts
  where
    repair = maybe 0 id mRepair
    parts = maybe 0 id mParts

foreign import ccall "Xfixes.h XFixesCreateRegionFromPicture"
    createRegion_ :: Ptr Display -> Picture -> IO Region

createRegion :: Display -> Picture -> IO Region
createRegion (Display pDpy) pict = createRegion_ pDpy pict

foreign import ccall "Xfixes.h XFixesDestroyRegion"
    destroyRegion_ :: Ptr Display -> Region -> IO ()

destroyRegion :: Display -> Region -> IO ()
destroyRegion (Display pDpy) region = destroyRegion_ pDpy region

foreign import ccall "Xfixes.h XFixesUnionRegion"
    unionRegion_ :: Ptr Display -> Region -> Region -> Region -> IO ()

unionRegion :: Display -> Region -> Region -> Region -> IO ()
unionRegion (Display pDpy) a b c = unionRegion_ pDpy a b c

foreign import ccall "Xfixes.h XFixesIntersectRegion"
    intersectRegion_ :: Ptr Display -> Region -> Region -> Region -> IO ()

intersectRegion :: Display -> Region -> Region -> Region -> IO ()
intersectRegion (Display pDpy) a b c = intersectRegion_ pDpy a b c

foreign import ccall "Xfixes.h XFixesTranslateRegion"
    translateRegion_ :: Ptr Display -> Region -> CInt -> CInt -> IO ()

translateRegion :: Display -> Region -> Int -> Int -> IO ()
translateRegion (Display pDpy) region x y =
    translateRegion_ pDpy region (fromIntegral x) (fromIntegral y)

foreign import ccall "Xrender.h XRenderSetPictureClipRegion"
    setPictureClipRegion_ :: Ptr Display -> Picture -> Region -> IO ()

setPictureClipRegion :: Display -> Picture -> Region -> IO ()
setPictureClipRegion (Display pDpy) pict region =
    setPictureClipRegion_ pDpy pict region

foreign import ccall "shape.h XShapeCombineRectangles"
    shapeRectangles_ :: Ptr Display -> Window -> CInt -> CInt -> CInt
                     -> Ptr Rectangle -> CInt -> CInt -> CInt -> IO ()

shapeRectangles :: Display -> Window -> CInt -> (CInt, CInt) -> [Rectangle]
                -> CInt -> CInt -> IO ()
shapeRectangles (Display pDpy) win kind (x, y) rects op ord =
    withArrayLen rects $ \nRects pRects ->
    shapeRectangles_ pDpy win kind x y pRects (fromIntegral nRects) op ord

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
