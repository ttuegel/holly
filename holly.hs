module Main where

import Control.Applicative ( (<$>) )
import Control.Concurrent ( forkIO )
import Control.Monad ( forever, unless, void )
import Graphics.XHB
import qualified Graphics.XHB.Gen.Composite as Composite
    ( extension, queryVersion, QueryVersionReply(..) )
import Graphics.XHB.Gen.Composite
import Graphics.XHB.Connection.Extension
import Graphics.XHB.Connection.Open
import qualified Graphics.XHB.Gen.Render as Render
    ( extension, queryVersion, QueryVersionReply(..) )
import Graphics.XHB.Gen.Render

main :: IO ()
main = do
    dpy <- maybe (error "Could not open display!") id <$> connect
    checkExtensions dpy
    errorHandler dpy
    eventHandler dpy
    paint dpy

getReply' :: Receipt a -> IO a
getReply' rcpt = do
    reply <- getReply rcpt
    case reply of
        Left err -> error $ show err
        Right a -> return a

checkExtensions :: Connection -> IO ()
checkExtensions dpy = do
    compositePresent <- extensionPresent dpy Composite.extension
    unless compositePresent $ error "Composite extension missing!"
    compositeVersion <- Composite.queryVersion dpy 0 3 >>= getReply'
    let compositeVersionOk = 
               Composite.major_version_QueryVersionReply compositeVersion > 0
            || Composite.minor_version_QueryVersionReply compositeVersion >= 3
    unless compositeVersionOk
        $ error "Composite extension version >= 0.3 required!"

    renderPresent <- extensionPresent dpy Render.extension
    unless renderPresent $ error "Render extension required!"
    renderVersion <- Render.queryVersion dpy 0 11 >>= getReply'
    let renderVersionOk =
               Render.major_version_QueryVersionReply renderVersion > 0
            || Render.minor_version_QueryVersionReply renderVersion >= 11
    unless renderVersionOk
        $ error "Render extension version >= 0.11 required!"

errorHandler :: Connection -> IO ()
errorHandler dpy = do
    void $ forkIO $ forever $ do
        err <- waitForError dpy
        putStrLn $ show err

eventHandler :: Connection -> IO ()
eventHandler dpy = do
    void $ forkIO $ forever $ do
        ev <- waitForEvent dpy
        return ()

paint :: Connection -> IO ()
paint dpy = do
    let scrNum = screen $ displayInfo dpy
        scr = (!! scrNum) $ roots_Setup $ connectionSetup dpy
        root = root_SCREEN scr
    blackPicture <- solidPicture dpy (toDrawable root) 0 0 0 0
    geom <- getGeometry dpy (toDrawable root) >>= getReply'
    let rootW = width_GetGeometryReply geom
        rootH = height_GetGeometryReply geom
    redirectSubwindows dpy root RedirectManual

    let rootVisual = root_visual_SCREEN scr
    format <- findVisualFormat dpy rootVisual

    overlayWindow <- getOverlayWindow dpy root >>= getReply'
    overlayPicture <- newResource dpy
    createPicture dpy $ MkCreatePicture
        { pid_CreatePicture = overlayPicture
        , drawable_CreatePicture = toDrawable overlayWindow
        , format_CreatePicture = format
        , value_CreatePicture =
            toValueParam [( CPSubwindowMode
                          , toValue SubwindowModeIncludeInferiors
                          )]
        }
    composite dpy $ MkComposite
        { op_Composite = PictOpSrc
        , src_Composite = blackPicture
        , mask_Composite = fromXid xidNone
        , dst_Composite = overlayPicture
        , src_x_Composite = 0
        , src_y_Composite = 0
        , mask_x_Composite = 0
        , mask_y_Composite = 0
        , dst_x_Composite = 0
        , dst_y_Composite = 0
        , width_Composite = rootW
        , height_Composite = rootH
        }

    forever $ do
        children <- children_QueryTreeReply
            <$> (queryTree dpy root >>= getReply')
        attrs <- mapM ((>>= getReply') . getWindowAttributes dpy) children
        let mappedChildren = filter ( (== MapStateViewable)
                                    . map_state_GetWindowAttributesReply
                                    . snd
                                    ) $ zip children attrs
            draw (win, attrs) = do
                pixm <- newResource dpy
                nameWindowPixmap dpy win pixm
                fmt <- findVisualFormat dpy
                    $ visual_GetWindowAttributesReply attrs
                geom <- getGeometry dpy (toDrawable win) >>= getReply'
                let b = border_width_GetGeometryReply geom
                pict <- newResource dpy
                createPicture dpy $ MkCreatePicture
                    { pid_CreatePicture = pict
                    , drawable_CreatePicture = toDrawable pixm
                    , format_CreatePicture = fmt
                    , value_CreatePicture =
                        toValueParam [(CPSubwindowMode, 1)]
                    }
                composite dpy $! MkComposite
                    { op_Composite = PictOpSrc
                    , src_Composite = pict
                    , mask_Composite = fromXid xidNone
                    , dst_Composite = overlayPicture
                    , src_x_Composite = 0
                    , src_y_Composite = 0
                    , mask_x_Composite = 0
                    , mask_y_Composite = 0
                    , dst_x_Composite = x_GetGeometryReply geom
                    , dst_y_Composite = y_GetGeometryReply geom
                    , width_Composite = width_GetGeometryReply geom + b + b
                    , height_Composite = height_GetGeometryReply geom + b + b
                    }
        mapM_ draw mappedChildren

class XidLike d => Drawable d where
    toDrawable :: d -> DRAWABLE
    toDrawable = fromXid . toXid

instance Drawable PIXMAP

instance Drawable WINDOW

solidPicture :: Connection -> DRAWABLE -> Double -> Double -> Double
             -> Double -> IO PICTURE
solidPicture dpy draw a r g b = do
    pixmap <- newResource dpy
    createPixmap dpy $ MkCreatePixmap
        { depth_CreatePixmap = 32
        , pid_CreatePixmap = pixmap
        , drawable_CreatePixmap = draw
        , width_CreatePixmap = 1
        , height_CreatePixmap = 1
        }

    formats <- formats_QueryPictFormatsReply
        <$> (queryPictFormats dpy >>= getReply')
    let format = id_PICTFORMINFO $ head
            $ filter ((== 32) . depth_PICTFORMINFO) formats

    picture <- newResource dpy
    createPicture dpy $ MkCreatePicture
        { pid_CreatePicture = picture
        , drawable_CreatePicture = toDrawable pixmap
        , format_CreatePicture = format
        , value_CreatePicture = toValueParam [(CPRepeat, 1)]
        }
    fillRectangles dpy $ MkFillRectangles
        { op_FillRectangles = PictOpSrc
        , dst_FillRectangles = picture
        , color_FillRectangles = MkCOLOR
            { red_COLOR = round $ r * 0xffff
            , green_COLOR = round $ g * 0xffff
            , blue_COLOR = round $ b * 0xffff
            , alpha_COLOR = round $ a * 0xffff
            }
        , rects_FillRectangles =
            [ MkRECTANGLE
                { x_RECTANGLE = 0
                , y_RECTANGLE = 0
                , width_RECTANGLE = 1
                , height_RECTANGLE = 1
                }
            ]
        }
    freePixmap dpy pixmap
    return picture

findVisualFormat :: Connection -> VISUALID -> IO PICTFORMAT
findVisualFormat dpy vid = do
    formats <- screens_QueryPictFormatsReply
        <$> (queryPictFormats dpy >>= getReply')
    return $! format_PICTVISUAL $ head
            $ filter ((== vid) . visual_PICTVISUAL)
            $ concatMap visuals_PICTDEPTH
            $ concatMap depths_PICTSCREEN formats

instance Eq MapState where
    a == b = toValue a == toValue b
