module Holly.Paint
    ( paint
    ) where

import Control.Applicative ( (<$>) )
import Control.Monad ( void )
import Data.Bits ( shiftL )
import Data.Maybe ( isJust )
import Data.Traversable ( mapM )
import Prelude hiding ( mapM )

import Graphics.XHB
import qualified Graphics.XHB.Gen.Damage as Damage
import Graphics.XHB.Gen.Render
import Graphics.XHB.Gen.XFixes

import Holly.Drawable
import Holly.Missing
import Holly.Types

paint :: Connection -> HollyState -> IO ()
paint dpy holly = do
    let overlay = overlayPicture holly
        buffer = bufferPicture holly

    let overlayDamage = extraRepaint holly
        applyWindowDamage win = do
            region <- newResource dpy
            createRegion dpy region []
            Damage.subtract dpy $! Damage.MkSubtract
                (winDamage win) (fromXid xidNone) region
            let b = fromIntegral $! winB win
            translateRegion dpy $! MkTranslateRegion region
                (winX win - b) (winY win - b)
            unionRegion dpy $! MkUnionRegion region overlayDamage overlayDamage
            destroyRegion dpy region
    void $ mapM applyWindowDamage $ wins holly
    setPictureClipRegion dpy $! MkSetPictureClipRegion buffer overlayDamage 0 0
    setPictureClipRegion dpy $! MkSetPictureClipRegion overlay overlayDamage 0 0
    setRegion dpy overlayDamage []

    let draw win = do
            mask <- solidPicture dpy (toDrawable $ overlayWindow holly)
                (winOpacity win) Nothing
            simpleComposite
                dpy PictOpOver (winPicture win) (Just mask) buffer
                (winX win, winY win)
                (winW win + winB win + winB win, winH win + winB win + winB win)
            freePicture dpy mask

    mRootPixmap <- getRootPixmap dpy $ root holly
    case mRootPixmap of
        Nothing -> return ()
        Just rootPixmap -> do
            rootPicture <- newResource dpy
            createPicture dpy $! MkCreatePicture
                { pid_CreatePicture = rootPicture
                , drawable_CreatePicture = toDrawable rootPixmap
                , format_CreatePicture = rootFormat holly
                , value_CreatePicture = emptyValueParam
                }
            simpleComposite dpy PictOpSrc rootPicture Nothing
                            buffer (0, 0) (rootW holly, rootH holly)
            freePicture dpy rootPicture

    void $ mapM draw $ wins holly

    simpleComposite dpy PictOpSrc buffer Nothing
                    overlay (0, 0) (rootW holly, rootH holly)

-- Painting Utilities ---------------------------------------------------

simpleComposite
    :: Connection
    -> PictOp           -- operation
    -> PICTURE          -- source
    -> Maybe PICTURE    -- mask
    -> PICTURE          -- destination
    -> (Int16, Int16)   -- (x, y)
    -> (Word16, Word16) -- (width, height)
    -> IO ()
simpleComposite dpy op src mask dest (x, y) (w, h) =
    composite dpy $! MkComposite
        { op_Composite = op
        , src_Composite = src
        , mask_Composite = maybe (fromXid xidNone) id mask
        , dst_Composite = dest
        , src_x_Composite = 0
        , src_y_Composite = 0
        , mask_x_Composite = 0
        , mask_y_Composite = 0
        , dst_x_Composite = x
        , dst_y_Composite = y
        , width_Composite = w
        , height_Composite = h
        }

solidPicture :: Connection -> DRAWABLE -> Double
             -> Maybe (Double, Double, Double) -> IO PICTURE
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

getRootPixmap :: Connection -> WINDOW -> IO (Maybe PIXMAP)
getRootPixmap dpy rootWindow = do
    rootPixmapAtom <- getAtom dpy "_XROOTPMAP_ID" False
    pixmapAtom <- getAtom dpy "PIXMAP" True

    pixmapIdBytes <- value_GetPropertyReply
        <$> ((getProperty dpy $! MkGetProperty
            { delete_GetProperty = False
            , window_GetProperty = rootWindow
            , property_GetProperty = rootPixmapAtom
            , type_GetProperty = pixmapAtom
            , long_offset_GetProperty = 0
            , long_length_GetProperty = 4
            }) >>= getReply')
    let shifts = map (* 8) [0..3]
        pixmapIdWords = zipWith shiftL (map fromIntegral pixmapIdBytes) shifts
        pixmapId :: Word32
        pixmapId = sum pixmapIdWords
    return $! if null pixmapIdBytes
        then Nothing
        else Just $! fromXid $! toXid pixmapId

