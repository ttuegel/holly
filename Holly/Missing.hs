{-# OPTIONS_GHC -fno-warn-orphans #-}
module Holly.Missing
    ( createBuffer
    , findStandardFormat
    , findVisualFormat
    , getAtom
    , getReply
    , getWindowOpacity
    ) where

import Control.Applicative ( (<$>) )
import Control.Monad.IO.Class

import Control.Error

import qualified Graphics.XHB as X
import Graphics.XHB hiding ( getReply )
import Graphics.XHB.Connection.Open ( screen )
import Graphics.XHB.Gen.Render

import Holly.Drawable
import Holly.Types

getReply :: MonadIO m => Receipt a -> EitherT SomeError m a
getReply receipt = liftIO (X.getReply receipt) >>= hoistEither

getAtom :: MonadIO m => Connection -> String -> Bool -> EitherT SomeError m ATOM
getAtom dpy name onlyIfExists = do
    liftIO requestAtom >>= getReply
  where
    requestAtom = internAtom dpy MkInternAtom
        { only_if_exists_InternAtom = onlyIfExists
        , name_len_InternAtom = fromIntegral $! length cname
        , name_InternAtom = cname
        }
    cname = stringToCList name

findVisualFormat :: MonadIO m => Connection -> VISUALID -> EitherT SomeError m PICTFORMAT
findVisualFormat dpy vid =
    format_PICTVISUAL . head
        . filter ((== vid) . visual_PICTVISUAL)
        <$> findScreenPictVisuals dpy

findStandardFormat :: MonadIO m => Connection -> Bool -> EitherT SomeError m PICTFORMAT
findStandardFormat dpy argb = do
    pictFormats <- liftIO (queryPictFormats dpy) >>= getReply
    return  $ id_PICTFORMINFO
            $ head
            $ filter ((== (if argb then 32 else 8)) . depth_PICTFORMINFO)
            $ filter ((== PictTypeDirect) . type_PICTFORMINFO)
            $ formats_QueryPictFormatsReply pictFormats

findScreenFormats :: MonadIO m => Connection -> EitherT SomeError m [PICTFORMAT]
findScreenFormats dpy = map format_PICTVISUAL <$> findScreenPictVisuals dpy

findScreenPictVisuals :: MonadIO m => Connection -> EitherT SomeError m [PICTVISUAL]
findScreenPictVisuals dpy = do
    pictFormats <- liftIO (queryPictFormats dpy) >>= getReply
    return  $ concatMap visuals_PICTDEPTH
            $ depths_PICTSCREEN
            $ (!! (screen $ displayInfo dpy))
            $ screens_QueryPictFormatsReply pictFormats

getWindowOpacity :: MonadIO m => Connection -> WINDOW -> EitherT SomeError m Double
getWindowOpacity dpy win = do
    cardinalAtom <- getAtom dpy "CARDINAL" True
    opacityAtom <- getAtom dpy "_NET_WM_WINDOW_OPACITY" False

    let propRequest = getProperty dpy MkGetProperty
            { delete_GetProperty      = False
            , window_GetProperty      = win
            , property_GetProperty    = opacityAtom
            , type_GetProperty        = cardinalAtom
            , long_offset_GetProperty = 0
            , long_length_GetProperty = 1
            }

    propReply <- liftIO propRequest >>= getReply
    let val = value_GetPropertyReply propReply

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

