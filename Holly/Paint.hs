module Holly.Paint
    ( paint
    ) where

import Control.Applicative ( (<$>) )
import Control.Monad ( void )
import Data.Bits ( shiftL )
import Data.Maybe ( isJust )
import Data.Traversable ( mapM )
import Prelude hiding ( mapM )

import Holly.Types
import Holly.X11

paint :: Display -> HollyState -> IO ()
paint dpy holly = do
    let overlay = overlayPicture holly
        buffer = bufferPicture holly

    let overlayDamage = extraRepaint holly
        applyWindowDamage win = do
            region <- createRegion dpy overlay
            damageSubtract dpy (winDamage win) Nothing (Just region)
            let b = fromIntegral $! winB win
            offsetRegion dpy region (winX win - b) (winY win - b)
            unionRegion dpy region overlayDamage overlayDamage
            destroyRegion dpy region
    void $ mapM applyWindowDamage $ wins holly
    setPictureClipRegion dpy buffer overlayDamage
    setPictureClipRegion dpy overlay overlayDamage
    emptyRegion <- createRegion dpy overlay
    intersectRegion overlayDamage emptyRegion emptyRegion
    destroyRegion emptyRegion

    let draw win = do
            mask <- solidPicture dpy (toDrawable $ overlayWindow holly)
                (winOpacity win) Nothing
            simpleComposite
                dpy PictOpOver (winPicture win) (Just mask) buffer
                (winX win, winY win)
                (winW win + winB win + winB win, winH win + winB win + winB win)
            freePicture dpy mask

    mRootPixmap <- getRootPixmap dpy $ root holly
    maybe (return ()) $ \rootPixmap -> do
        rootPicture <- createPicture dpy (toDrawable rootPixmap)
                                     (rootFormat holly) def
        simpleComposite dpy PictOpSrc rootPicture Nothing
                        buffer (0, 0) (rootW holly, rootH holly)
        freePicture dpy rootPicture

    void $ mapM draw $ wins holly

    simpleComposite dpy PictOpSrc buffer Nothing
                    overlay (0, 0) (rootW holly, rootH holly)

-- Painting Utilities ---------------------------------------------------

simpleComposite
    :: Display
    -> PictOp           -- operation
    -> Picture          -- source
    -> Maybe Picture    -- mask
    -> Picture          -- destination
    -> (Position, Position)   -- (x, y)
    -> (Dimension, Dimension) -- (width, height)
    -> IO ()
simpleComposite dpy op srcPict maskPict dstPict (x', y') (w, h) =
    composite dpy op src mask dst (w, h)
  where
    src = Located { x = 0, y = 0, val = srcPict }
    mask = fmap (\p -> Located { x = 0, y = 0, val = p }) maskPict
    dst = Located { x = x', y = y', val = dstPict }

solidPicture :: Display -> Drawable -> Double
             -> Maybe (Double, Double, Double) -> IO Picture
solidPicture dpy draw a mRGB = do
    format <- findStandardFormat dpy (isJust mRGB)
    picture <- createBuffer dpy draw (1, 1) (if isJust mRGB then 32 else 8) format

    let r = maybe 0.0 (\(x, _, _) -> x) mRGB
        g = maybe 0.0 (\(_, x, _) -> x) mRGB
        b = maybe 0.0 (\(_, _, x) -> x) mRGB
    fillRectangles dpy $ MkFillRectangles
        { op_FillRectangles = PictOpSrc
        , dst_FillRectangles = picture
        , color_FillRectangles = MkCOLOR
            { red_COLOR = round $ r * 0xffff
            , green_COLOR = round $ g * 0xffff
            , blue_COLOR = round $ b * 0xffff
            , alpha_COLOR = round $ a * 0xffff
            }
        , rects_FillRectangles = [ MkRECTANGLE 0 0 1 1 ]
        }

    return picture

getRootPixmap :: Display -> Window -> IO (Maybe Pixmap)
getRootPixmap dpy rootWindow = do
    rootPixmapAtom <- internAtom dpy "_XROOTPMAP_ID" False
    pixmapIdBytes <- getWindowProperty32 dpy rootPixmapAtom rootWindow
    return $! fmap (fromIntegral . head) pixmapIdBytes

