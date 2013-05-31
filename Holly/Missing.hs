{-# OPTIONS_GHC -fno-warn-orphans #-}
module Holly.Missing
    ( createBuffer
    , findStandardFormat
    , findVisualFormat
    , getAtom
    , getReply'
    , getWindowOpacity
    ) where

import Control.Applicative ( (<$>) )

import Graphics.XHB
import Graphics.XHB.Connection.Open ( screen )
import Graphics.XHB.Gen.Render

import Holly.Drawable
import Holly.Types

getReply' :: Receipt a -> IO a
getReply' rcpt = do
    reply <- getReply rcpt
    case reply of
        Left err -> error $ show err
        Right a -> return a

getAtom :: Connection -> String -> Bool -> IO ATOM
getAtom dpy name onlyIfExists =
    (internAtom dpy $! MkInternAtom
        { only_if_exists_InternAtom = onlyIfExists
        , name_len_InternAtom = fromIntegral $! length cname
        , name_InternAtom = cname
        }
    ) >>= getReply'
  where
    cname = stringToCList name

findVisualFormat :: Connection -> VISUALID -> IO PICTFORMAT
findVisualFormat dpy vid =
    format_PICTVISUAL . head
        . filter ((== vid) . visual_PICTVISUAL)
        <$> findScreenPictVisuals dpy

findStandardFormat :: Connection -> Bool -> IO PICTFORMAT
findStandardFormat dpy argb = do
    id_PICTFORMINFO . head
        . filter ((== (if argb then 32 else 8)) . depth_PICTFORMINFO)
        . filter ((== PictTypeDirect) . type_PICTFORMINFO)
        . formats_QueryPictFormatsReply
        <$> (queryPictFormats dpy >>= getReply')

findScreenFormats :: Connection -> IO [PICTFORMAT]
findScreenFormats dpy = map format_PICTVISUAL <$> findScreenPictVisuals dpy

findScreenPictVisuals :: Connection -> IO [PICTVISUAL]
findScreenPictVisuals dpy =
    concatMap visuals_PICTDEPTH . depths_PICTSCREEN
    . (!! (screen $ displayInfo dpy)) . screens_QueryPictFormatsReply
    <$> (queryPictFormats dpy >>= getReply')

getWindowOpacity :: Connection -> WINDOW -> IO Double
getWindowOpacity dpy win = do
    cardinalAtom <- getAtom dpy "CARDINAL" True
    opacityAtom <- getAtom dpy "_NET_WM_WINDOW_OPACITY" False

    val <- value_GetPropertyReply <$> ((getProperty dpy $ MkGetProperty
        { delete_GetProperty = False
        , window_GetProperty = win
        , property_GetProperty = opacityAtom
        , type_GetProperty = cardinalAtom
        , long_offset_GetProperty = 0
        , long_length_GetProperty = 1
        }) >>= getReply')
    return $! if null val
        then 1.0
        else fromIntegral (head val) / fromIntegral maxOpacity
  where
    maxOpacity :: Int
    maxOpacity = 0xff

createBuffer :: Connection -> DRAWABLE -> (Word16, Word16) -> Word8
             -> PICTFORMAT -> IO PICTURE
createBuffer dpy draw (w, h) depth format = do
    pixmap <- newResource dpy
    createPixmap dpy $! MkCreatePixmap depth pixmap draw w h

    picture <- newResource dpy
    createPicture dpy $ MkCreatePicture
        { pid_CreatePicture = picture
        , drawable_CreatePicture = toDrawable pixmap
        , format_CreatePicture = format
        , value_CreatePicture = toValueParam
            [(CPRepeat, toValue RepeatNormal)]
        }

    freePixmap dpy pixmap
    return picture


instance Eq MapState where
    a == b = toValue a == (toValue b :: Integer)

instance Eq PictType where
    a == b = toValue a == (toValue b :: Integer)

instance Eq WindowClass where
    a == b = toValue a == (toValue b :: Integer)

