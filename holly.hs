{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Control.Applicative ( (<$>) )
import Control.Concurrent ( forkIO )
import Control.Monad ( forever, unless, void, when )
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Bits ( shiftL )
import Data.Int ( Int16 )
import Data.Maybe ( catMaybes, fromJust, isJust )
import Data.Sequence ( Seq )
import Data.Traversable ( mapM )
import qualified Data.Sequence as S
import Data.Word ( Word8, Word16, Word32 )
import Graphics.XHB
import qualified Graphics.XHB.Gen.Composite as Composite
    ( extension, queryVersion, QueryVersionReply(..) )
import Graphics.XHB.Gen.Composite
import Graphics.XHB.Connection.Extension
import Graphics.XHB.Connection.Open
import qualified Graphics.XHB.Gen.Damage as Damage
import qualified Graphics.XHB.Gen.Render as Render
    ( extension, queryVersion, QueryVersionReply(..) )
import Graphics.XHB.Gen.Render
import qualified Graphics.XHB.Gen.Shape as Shape
    ( extension, queryVersion, QueryVersionReply(..) )
import Graphics.XHB.Gen.Shape hiding ( mask )
import qualified Graphics.XHB.Gen.XFixes as XFixes
    ( extension, queryVersion, QueryVersionReply(..) )
import Graphics.XHB.Gen.XFixes
import Prelude hiding ( init, mapM )

main :: IO ()
main = do
    dpy <- maybe (error "Could not open display!") id <$> connect
    errorHandler dpy
    checkExtensions dpy
    init dpy >>= evalStateT (eventHandler dpy)

errorHandler :: Connection -> IO ()
errorHandler dpy = void $ forkIO $ do
    let extensions =
            [ Composite.extension
            , Render.extension
            , Damage.extension
            , XFixes.extension
            , Shape.extension
            ]
        names =
            [ "Composite"
            , "Render"
            , "Damage"
            , "Fixes"
            , "Shape"
            ]
    opCodes <- mapM (extensionOpCode dpy) extensions
    let extOpCodes = zip opCodes names
    forever $ do
        err <- waitForError dpy
        case fromError err of
            Nothing -> return ()
            Just match -> case lookup (major_opcode_MatchError match) extOpCodes of
                Nothing -> return ()
                Just name -> putStr $ name ++ ": "
        putStrLn $ show err

data HollyState = HollyState
    { wins  :: Seq Win
    , root  :: WINDOW
    , scr   :: SCREEN
    , rootW :: Word16
    , rootH :: Word16
    , rootFormat :: PICTFORMAT
    , overlayPicture :: PICTURE
    , overlayWindow :: WINDOW
    , bufferPicture :: PICTURE
    , extraRepaint  :: REGION
    }

init :: Connection -> IO HollyState
init dpy = do
    let scrNum = screen $ displayInfo dpy
        s = (!! scrNum) $ roots_Setup $ connectionSetup dpy
        r = root_SCREEN s
    geom <- getGeometry dpy (toDrawable r) >>= getReply'
    redirectSubwindows dpy r RedirectManual
    let rootVisual = root_visual_SCREEN s
    rootF <- findVisualFormat dpy rootVisual

    overlayW <- getOverlayWindow dpy r >>= getReply'
    overlayP <- newResource dpy
    createPicture dpy $! MkCreatePicture
        { pid_CreatePicture = overlayP
        , drawable_CreatePicture = toDrawable overlayW
        , format_CreatePicture = rootF
        , value_CreatePicture = toValueParam
            [( CPSubwindowMode
             , toValue SubwindowModeIncludeInferiors
             )]
        }
    rectangles dpy $! MkRectangles
        { operation_Rectangles = SOSet
        , destination_kind_Rectangles = SKInput
        , ordering_Rectangles = ClipOrderingUnsorted
        , destination_window_Rectangles = overlayW
        , x_offset_Rectangles = 0
        , y_offset_Rectangles = 0
        , rectangles_Rectangles = []
        }

    changeWindowAttributes dpy r $! toValueParam
        [(CWEventMask, toMask
            [ EventMaskSubstructureNotify
            , EventMaskExposure
            , EventMaskStructureNotify
            , EventMaskPropertyChange
            ]
         )]
    selectInput dpy r True

    children <- children_QueryTreeReply <$> (queryTree dpy r >>= getReply')
    ws <- catMaybes <$> mapM (getWindow dpy) children

    let w = width_GetGeometryReply geom
        h = height_GetGeometryReply geom
    bufferFormat <- findStandardFormat dpy True
    buffer <- createBuffer dpy
        (toDrawable overlayW)
        (w, h)
        32 bufferFormat

    extra <- newResource dpy
    createRegion dpy extra [ MkRECTANGLE 0 0 w h ]

    return $! HollyState
        { wins = S.fromList $! ws
        , scr = s
        , root = r
        , rootW = w
        , rootH = h
        , rootFormat = rootF
        , overlayPicture = overlayP
        , overlayWindow = overlayW
        , bufferPicture = buffer
        , extraRepaint = extra
        }

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

    damagePresent <- extensionPresent dpy Damage.extension
    unless damagePresent
        $ error "Damage extension required!"
    damageVersion <- Damage.queryVersion dpy 1 1 >>= getReply'
    let damageVersionOk =
            (   Damage.major_version_QueryVersionReply damageVersion == 1
             && Damage.minor_version_QueryVersionReply damageVersion >= 1
            ) || Damage.major_version_QueryVersionReply damageVersion > 1
    unless damageVersionOk
        $ error "Damage extension version >= 1.1 required!"

eventHandler :: Connection -> StateT HollyState IO ()
eventHandler dpy = forever $ do
    st <- get
    liftIO $ paint dpy st
    ev <- liftIO $ waitForEvent dpy
    let handlers =
            [ createNotifyHandler dpy
            , configureNotifyHandler dpy
            , destroyNotifyHandler dpy $ extraRepaint st
            , mapNotifyHandler dpy
            , unmapNotifyHandler dpy $ extraRepaint st
            , reparentNotifyHandler dpy (extraRepaint st) $ root st
            , circulateNotifyHandler
            , propertyNotifyHandler dpy
            ]
        handler e = do
            mapM_ (\f -> f e) handlers
            mE <- liftIO $ pollForEvent dpy
            case mE of
                Nothing -> return ()
                Just e' -> handler e'
    wins' <- liftIO $ execStateT (handler ev) $ wins st
    modify $ \s -> s { wins = wins' }

guarded :: (Event e, Monad m) => (e -> m ()) -> SomeEvent -> m ()
guarded f ev = let ev' = fromEvent ev in when (isJust ev') $ f $ fromJust ev'

findWindowIx :: WINDOW -> (Int -> StateT (Seq Win) IO ()) -> StateT (Seq Win) IO ()
findWindowIx wid go = do
    mIx <- gets $ S.findIndexL ((== wid) . winId)
    case mIx of
        Nothing -> return ()
        Just ix -> go ix

updateWindow :: Connection -> Win -> StateT (Seq Win) IO ()
updateWindow dpy new = findWindowIx (winId new) $ \oldIx -> do
    gets (flip S.index oldIx) >>= freeWindow dpy
    modify $ S.update oldIx new

freeWindow :: Connection -> Win -> StateT (Seq Win) IO ()
freeWindow dpy win = liftIO $ do
    Damage.destroy dpy $ winDamage win
    freePicture dpy $ winPicture win

createNotifyHandler :: Connection -> SomeEvent -> StateT (Seq Win) IO ()
createNotifyHandler dpy = guarded $ \ev -> do
    mWin <- liftIO $ getWindow dpy $ window_CreateNotifyEvent ev
    case mWin of
        Nothing -> return ()
        Just win -> modify (S.|> win)

discardWindow :: Connection -> REGION -> WINDOW -> StateT (Seq Win) IO ()
discardWindow dpy extra wid = findWindowIx wid $ \ix -> do
    win <- gets $ flip S.index ix
    let x = winX win
        y = winY win
        w = winW win
        h = winH win
        b = winB win
    liftIO $ do
        winRegion <- newResource dpy
        createRegion dpy winRegion
            [ MkRECTANGLE x y (w + b + b) (h + b + b) ]
        unionRegion dpy $! MkUnionRegion winRegion extra extra
        destroyRegion dpy winRegion
    freeWindow dpy win
    modify $ S.filter ((/= wid) . winId)

configureNotifyHandler :: Connection -> SomeEvent -> StateT (Seq Win) IO ()
configureNotifyHandler dpy = guarded $ \ev -> do
    let wid = window_ConfigureNotifyEvent ev
    liftIO (getWindow dpy wid) >>= maybe (return ()) (updateWindow dpy)

destroyNotifyHandler :: Connection -> REGION -> SomeEvent -> StateT (Seq Win) IO ()
destroyNotifyHandler dpy extra = guarded $ \ev -> do
    let wid = window_DestroyNotifyEvent ev
    discardWindow dpy extra wid

unmapNotifyHandler :: Connection -> REGION -> SomeEvent -> StateT (Seq Win) IO ()
unmapNotifyHandler dpy extra = guarded $ \ev -> do
    let wid = window_UnmapNotifyEvent ev
    discardWindow dpy extra wid

mapNotifyHandler :: Connection -> SomeEvent -> StateT (Seq Win) IO ()
mapNotifyHandler dpy = guarded $ \ev -> do
    mWin <- liftIO $ getWindow dpy $ window_MapNotifyEvent ev
    case mWin of
        Nothing -> return ()
        Just win -> modify (S.|> win)

reparentNotifyHandler :: Connection -> REGION -> WINDOW -> SomeEvent -> StateT (Seq Win) IO ()
reparentNotifyHandler dpy extra rootWindow = guarded $ \ev -> do
    let wid = window_ReparentNotifyEvent ev
    if parent_ReparentNotifyEvent ev == rootWindow
        then liftIO (getWindow dpy wid)
            >>= maybe (return ()) (\win -> modify (S.|> win))
        else discardWindow dpy extra wid

getWindow :: Connection -> WINDOW -> IO (Maybe Win)
getWindow dpy wid = do
    attrs <- getWindowAttributes dpy wid >>= getReply'
    if (inputOnly attrs || not (viewable attrs))
        then return Nothing
        else do
            geom <- getGeometry dpy (toDrawable wid) >>= getReply'
            opacity <- getWindowOpacity dpy wid
            fmt <- findVisualFormat dpy $ visual_GetWindowAttributesReply attrs
            changeWindowAttributes dpy wid $! toValueParam
                [(CWEventMask, toMask [ EventMaskPropertyChange ])]
            dam <- newResource dpy
            Damage.create dpy $! Damage.MkCreate
                { Damage.damage_Create = dam
                , Damage.drawable_Create = toDrawable wid
                , Damage.level_Create = Damage.ReportLevelNonEmpty
                }
            pixmap <- newResource dpy
            nameWindowPixmap dpy wid pixmap
            pict <- newResource dpy
            createPicture dpy $ MkCreatePicture
                { pid_CreatePicture = pict
                , drawable_CreatePicture = toDrawable pixmap
                , format_CreatePicture = fmt
                , value_CreatePicture = toValueParam
                    [( CPSubwindowMode
                     , toValue SubwindowModeIncludeInferiors
                     )]
                }
            freePixmap dpy pixmap
            let win = Win
                    { winX = x_GetGeometryReply geom
                    , winY = y_GetGeometryReply geom
                    , winW = width_GetGeometryReply geom
                    , winH = height_GetGeometryReply geom
                    , winB = border_width_GetGeometryReply geom
                    , winId = wid
                    , winFormat = fmt
                    , winOpacity = opacity
                    , winDamage = dam
                    , winPicture = pict
                    }
            damageWholeWindow dpy win
            return $! Just $! win
  where
    viewable = (== MapStateViewable) . map_state_GetWindowAttributesReply
    inputOnly = (== WindowClassInputOnly) . class_GetWindowAttributesReply

damageWholeWindow :: Connection -> Win -> IO ()
damageWholeWindow dpy win = do
    region <- newResource dpy
    createRegion dpy region [ MkRECTANGLE 0 0 (winW win) (winH win) ]
    Damage.add dpy (toDrawable $ winId win) region
    destroyRegion dpy region

circulateNotifyHandler :: SomeEvent -> StateT (Seq Win) IO ()
circulateNotifyHandler = guarded $ \ev -> modify $ \ws -> do
    let wid = window_CirculateNotifyEvent ev
        ws' = S.filter ((/= wid) . winId) ws
    case S.findIndexL ((== wid) . winId) ws of
        Nothing -> ws
        Just wIx ->
            let win = S.index ws wIx
            in case place_CirculateNotifyEvent ev of
                PlaceOnTop -> win S.<| ws'
                PlaceOnBottom -> ws' S.|> win

propertyNotifyHandler :: Connection -> SomeEvent -> StateT (Seq Win) IO ()
propertyNotifyHandler dpy = guarded $ \ev -> do
    let wid = window_PropertyNotifyEvent ev
    opacityAtom <- liftIO $ getAtom dpy "_NET_WM_WINDOW_OPACITY" False
    if atom_PropertyNotifyEvent ev == opacityAtom
        then do
            newOpacity <- liftIO $ getWindowOpacity dpy wid
            findWindowIx wid $ \ix -> do
                win <- gets $ flip S.index ix
                liftIO $ damageWholeWindow dpy win
                modify $ S.update ix $ win { winOpacity = newOpacity }
        else return ()

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
                (winX win + b) (winY win + b)
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
    a == b = toValue a == (toValue b :: Integer)

instance Eq PictType where
    a == b = toValue a == (toValue b :: Integer)

instance Eq WindowClass where
    a == b = toValue a == (toValue b :: Integer)

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

data Win = Win
    { winId :: WINDOW
    , winX  :: Int16
    , winY  :: Int16
    , winW  :: Word16
    , winH  :: Word16
    , winB  :: Word16
    , winFormat :: PICTFORMAT
    , winOpacity :: Double
    , winDamage :: Damage.DAMAGE
    , winPicture    :: PICTURE
    }
  deriving (Eq, Show)
