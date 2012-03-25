module Main where

import Control.Applicative ( (<$>) )
import Control.Concurrent ( forkIO )
import Control.Monad ( forever, sequence_, unless, void )
import Data.Bits ( shiftL )
import Data.Int ( Int16 )
import Data.Maybe ( isJust )
import Data.Word ( Word8, Word16, Word32 )
import Graphics.XHB
import qualified Graphics.XHB.Gen.Composite as Composite
    ( extension, queryVersion, QueryVersionReply(..) )
import Graphics.XHB.Gen.Composite
import Graphics.XHB.Connection.Extension
import Graphics.XHB.Connection.Open
import qualified Graphics.XHB.Gen.Render as Render
    ( extension, queryVersion, QueryVersionReply(..) )
import Graphics.XHB.Gen.Render
import qualified Graphics.XHB.Gen.Shape as Shape
    ( extension, queryVersion, QueryVersionReply(..) )
import Graphics.XHB.Gen.Shape
import qualified Graphics.XHB.Gen.XFixes as XFixes
    ( extension, queryVersion, QueryVersionReply(..) )
import Graphics.XHB.Gen.XFixes

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

    fixesPresent <- extensionPresent dpy XFixes.extension
    unless fixesPresent $ error "XFixes extension required!"
    fixesVersion <- XFixes.queryVersion dpy 2 0 >>= getReply'
    let fixesVersionOk =
            XFixes.major_version_QueryVersionReply fixesVersion >= 2
    unless fixesVersionOk
        $ error "XFixes extension version >= 2 required!"
            
    shapePresent <- extensionPresent dpy Shape.extension
    unless shapePresent $ error "Shape extension required!"
    shapeVersion <- Shape.queryVersion dpy >>= getReply'
    let shapeVersionOk =
            (   Shape.major_version_QueryVersionReply shapeVersion == 1
             && Shape.minor_version_QueryVersionReply shapeVersion >= 1
            ) || Shape.major_version_QueryVersionReply shapeVersion > 1
    unless shapeVersionOk
        $ error "Shape extension version >= 1 required!"

errorHandler :: Connection -> IO ()
errorHandler dpy = do
    void $ forkIO $ forever $ do
        err <- waitForError dpy
        putStrLn $ show err

eventHandler :: Connection -> IO ()
eventHandler dpy = do
    void $ forkIO $ forever $ do
        ev <- waitForEvent dpy
        case fromEvent ev of
            Nothing -> return ()
            Just (MkButtonPressEvent {}) ->
                putStrLn "Got a button press event, but I shouldn't be getting button press events!"
  where
    root = root_SCREEN scr
    scr = (!! scrNum) $ roots_Setup $ connectionSetup dpy
    scrNum = screen $ displayInfo dpy

paint :: Connection -> IO ()
paint dpy = do
    let scrNum = screen $ displayInfo dpy
        scr = (!! scrNum) $ roots_Setup $ connectionSetup dpy
        root = root_SCREEN scr
    geom <- getGeometry dpy (toDrawable root) >>= getReply'
    let rootW = width_GetGeometryReply geom
        rootH = height_GetGeometryReply geom
    redirectSubwindows dpy root RedirectManual

    let rootVisual = root_visual_SCREEN scr
    rootFormat <- findVisualFormat dpy rootVisual

    overlayWindow <- getOverlayWindow dpy root >>= getReply'
    overlayPicture <- newResource dpy
    createPicture dpy $! MkCreatePicture
        { pid_CreatePicture = overlayPicture
        , drawable_CreatePicture = toDrawable overlayWindow
        , format_CreatePicture = rootFormat
        , value_CreatePicture = toValueParam
            [( CPSubwindowMode
             , toValue SubwindowModeIncludeInferiors
             )]
        }

    changeWindowAttributes dpy overlayWindow $! toValueParam
        [(CWEventMask, toMask
            [ EventMaskSubstructureNotify
            , EventMaskExposure
            , EventMaskStructureNotify
            , EventMaskPropertyChange
            ]
         )]

    rectangles dpy $! MkRectangles
        { operation_Rectangles = SOSet
        , destination_kind_Rectangles = SKInput
        , ordering_Rectangles = ClipOrderingUnsorted
        , destination_window_Rectangles = overlayWindow
        , x_offset_Rectangles = 0
        , y_offset_Rectangles = 0
        , rectangles_Rectangles = []
        }

    forever $ do
        bufferFormat <- findStandardFormat dpy True
        buffer <- createBuffer dpy (toDrawable overlayWindow) (rootW, rootH)
                  32 bufferFormat

        children <- children_QueryTreeReply
            <$> (queryTree dpy root >>= getReply')
        childrenAttrs <- mapM (getWindowAttributes dpy) children >>= mapM getReply'
        let childrenWithAttrs = filter viewable . filter (not . inputOnly)
                $ zip children childrenAttrs
            viewable =   (== MapStateViewable)
                       . map_state_GetWindowAttributesReply
                       . snd
            inputOnly =   (== WindowClassInputOnly)
                       . class_GetWindowAttributesReply
                       . snd
            draw (child, attrs) = do
                pixm <- newResource dpy
                nameWindowPixmap dpy child pixm
                childFormat <- findVisualFormat dpy $! visual_GetWindowAttributesReply attrs
                geom <- getGeometry dpy (toDrawable child) >>= getReply'
                let b = border_width_GetGeometryReply geom
                    w = width_GetGeometryReply geom
                    h = height_GetGeometryReply geom
                opacity <- getWindowOpacity dpy child
                mask <- solidPicture dpy (toDrawable overlayWindow) opacity Nothing
                pict <- newResource dpy
                createPicture dpy $ MkCreatePicture
                    { pid_CreatePicture = pict
                    , drawable_CreatePicture = toDrawable pixm
                    , format_CreatePicture = childFormat
                    , value_CreatePicture = toValueParam
                        [( CPSubwindowMode
                         , toValue SubwindowModeIncludeInferiors
                         )]
                    }
                simpleComposite
                    dpy PictOpOver pict (Just mask) buffer
                    (x_GetGeometryReply geom, y_GetGeometryReply geom)
                    (w + b + b, h + b + b)
                freePicture dpy pict

        mRootPixmap <- getRootPixmap dpy root
        case mRootPixmap of
            Nothing -> return ()
            Just rootPixmap -> do
                rootPicture <- newResource dpy
                createPicture dpy $! MkCreatePicture
                    { pid_CreatePicture = rootPicture
                    , drawable_CreatePicture = toDrawable rootPixmap
                    , format_CreatePicture = rootFormat
                    , value_CreatePicture = emptyValueParam
                    }
                simpleComposite dpy PictOpSrc rootPicture Nothing
                                buffer (0, 0) (rootW, rootH)
                freePicture dpy rootPicture

        mapM_ draw childrenWithAttrs

        simpleComposite dpy PictOpOver buffer Nothing
                        overlayPicture (0, 0) (rootW, rootH)

        freePicture dpy buffer

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

class XidLike d => Drawable d where
    toDrawable :: d -> DRAWABLE
    toDrawable = fromXid . toXid

instance Drawable PIXMAP

instance Drawable WINDOW

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
        , rects_FillRectangles =
            [ MkRECTANGLE
                { x_RECTANGLE = 0
                , y_RECTANGLE = 0
                , width_RECTANGLE = 1
                , height_RECTANGLE = 1
                }
            ]
        }

    return picture

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
findScreenFormats dpy =
    map format_PICTVISUAL <$> findScreenPictVisuals dpy

findScreenPictVisuals :: Connection -> IO [PICTVISUAL]
findScreenPictVisuals dpy =
    concatMap visuals_PICTDEPTH . depths_PICTSCREEN
    . (!! (screen $ displayInfo dpy)) . screens_QueryPictFormatsReply
    <$> (queryPictFormats dpy >>= getReply')

instance Eq MapState where
    a == b = toValue a == toValue b

instance Eq PictType where
    a == b = toValue a == toValue b

instance Eq WindowClass where
    a == b = toValue a == toValue b

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
    return $ if null val then 1.0 else fromIntegral (head val) / fromIntegral 0xff

createBuffer :: Connection -> DRAWABLE -> (Word16, Word16) -> Word8
             -> PICTFORMAT -> IO PICTURE
createBuffer dpy draw (w, h) depth format = do
    pixmap <- newResource dpy
    createPixmap dpy $ MkCreatePixmap
        { depth_CreatePixmap = depth
        , pid_CreatePixmap = pixmap
        , drawable_CreatePixmap = draw
        , width_CreatePixmap = w
        , height_CreatePixmap = h
        }

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

getRootPixmap :: Connection -> WINDOW -> IO (Maybe PIXMAP)
getRootPixmap dpy root = do
    rootPixmapAtom <- getAtom dpy "_XROOTPMAP_ID" False
    pixmapAtom <- getAtom dpy "PIXMAP" True

    pixmapIdBytes <- value_GetPropertyReply
        <$> ((getProperty dpy $! MkGetProperty
            { delete_GetProperty = False
            , window_GetProperty = root
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
