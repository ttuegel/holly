module Holly.Paint
    ( paint
    ) where

import Control.Applicative ( (<$>) )
import Data.Bits ( shiftL )
import Data.Foldable ( mapM_ )
import Data.Maybe ( isJust )
import Prelude hiding ( mapM, mapM_ )

import Graphics.XHB
import qualified Graphics.XHB.Gen.Damage as Damage
import Graphics.XHB.Gen.Damage ( Subtract(..) )
import Graphics.XHB.Gen.Render
import Graphics.XHB.Gen.XFixes

import Holly.Drawable
import Holly.Missing
import Holly.Types

paint :: Connection -> HollyState -> IO ()
paint dpy holly = do
    let overlay               = overlayPicture holly
        buffer                = bufferPicture holly
        overlayDamage         = extraRepaint holly
        overlayWindowDrawable = toDrawable $ overlayWindow holly

    let getWindowDamage win = do
            damaged <- newResource dpy
            createRegion dpy damaged []
            -- Get the window's damaged region and mark the whole window as
            -- undamaged.
            Damage.subtract dpy MkSubtract
                { damage_Subtract = winDamage win
                , repair_Subtract = fromXid xidNone
                , parts_Subtract = damaged
                }
            -- Damage to the window is reported relative to the window's
            -- coordinates. Go to global coordinates by translating the
            -- damaged region.
            let b = fromIntegral $! winB win
            translateRegion dpy MkTranslateRegion
                { region_TranslateRegion = damaged
                , dx_TranslateRegion = winX win - b
                , dy_TranslateRegion = winY win - b
                }
            -- Add the damage from this window to the total damage.
            unionRegion dpy MkUnionRegion
                { source1_UnionRegion = damaged
                , source2_UnionRegion = overlayDamage
                , destination_UnionRegion = overlayDamage
                }
            destroyRegion dpy damaged
        
        simpleComposite = MkComposite
            { op_Composite     = PictOpSrc
            , src_Composite    = pictNone
            , mask_Composite   = pictNone
            , dst_Composite    = buffer
            , src_x_Composite  = 0
            , src_y_Composite  = 0
            , mask_x_Composite = 0
            , mask_y_Composite = 0
            , dst_x_Composite  = 0
            , dst_y_Composite  = 0
            , width_Composite  = 0
            , height_Composite = 0
            }

    -- Accumulate damage to all windows.
    mapM_ getWindowDamage $ wins holly
    -- Clip buffer and overlay to avoid repainting undamaged regions.
    setPictureClipRegion dpy MkSetPictureClipRegion
        { picture_SetPictureClipRegion  = buffer
        , region_SetPictureClipRegion   = overlayDamage
        , x_origin_SetPictureClipRegion = 0
        , y_origin_SetPictureClipRegion = 0
        }
    setPictureClipRegion dpy MkSetPictureClipRegion
        { picture_SetPictureClipRegion  = overlay
        , region_SetPictureClipRegion   = overlayDamage
        , x_origin_SetPictureClipRegion = 0
        , y_origin_SetPictureClipRegion = 0
        }
    -- Reset damage.
    setRegion dpy overlayDamage []

    let paintRootPixmap rootPixmap = do
            rootPicture <- newResource dpy
            createPicture dpy MkCreatePicture
                { pid_CreatePicture      = rootPicture
                , drawable_CreatePicture = toDrawable rootPixmap
                , format_CreatePicture   = rootFormat holly
                , value_CreatePicture    = emptyValueParam
                }
            composite dpy simpleComposite
                { src_Composite    = rootPicture
                , width_Composite  = rootW holly
                , height_Composite = rootH holly
                }
            freePicture dpy rootPicture

        -- Draw a window with the alpha mask appropriate to its opacity.
        draw win = do
            mask <- solidPicture dpy overlayWindowDrawable
                (winOpacity win) Nothing
            composite dpy simpleComposite
                { op_Composite     = PictOpOver
                , src_Composite    = winPicture win
                , mask_Composite   = mask
                , dst_x_Composite  = winX win
                , dst_y_Composite  = winY win
                , width_Composite  = winW win + (2 * winB win)
                , height_Composite = winH win + (2 * winB win)
                }
            freePicture dpy mask

    -- Paint to the buffer.
    getRootPixmap dpy (root holly) >>= mapM_ paintRootPixmap
    mapM_ draw $ wins holly

    -- Copy the buffer to the overlay window.
    composite dpy simpleComposite
        { src_Composite    = buffer
        , dst_Composite    = overlay
        , width_Composite  = rootW holly
        , height_Composite = rootH holly
        }

-- Painting Utilities ---------------------------------------------------

-- | Paint a 1px by 1px `PICTURE` of a solid color.
solidPicture :: Connection -> DRAWABLE -> Double
             -> Maybe (Double, Double, Double) -> IO PICTURE
solidPicture dpy draw a mRGB = do
    format <- findStandardFormat dpy (isJust mRGB)
    let depth | isJust mRGB = 32
              | otherwise   = 8
    picture <- createBuffer dpy draw (1, 1) depth format

    let r = maybe 0.0 (\(x, _, _) -> x) mRGB
        g = maybe 0.0 (\(_, x, _) -> x) mRGB
        b = maybe 0.0 (\(_, _, x) -> x) mRGB
    fillRectangles dpy $ MkFillRectangles
        { op_FillRectangles    = PictOpSrc
        , dst_FillRectangles   = picture
        , color_FillRectangles = MkCOLOR
            { red_COLOR   = round $ r * 0xffff
            , green_COLOR = round $ g * 0xffff
            , blue_COLOR  = round $ b * 0xffff
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
        <$> ((getProperty dpy MkGetProperty
            { delete_GetProperty      = False
            , window_GetProperty      = rootWindow
            , property_GetProperty    = rootPixmapAtom
            , type_GetProperty        = pixmapAtom
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

pictNone :: PICTURE
pictNone = fromXid xidNone
