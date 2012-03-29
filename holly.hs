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

-- Main Loop ------------------------------------------------------------

main :: IO ()
main = do
    dpy <- maybe (error "Could not open display!") id <$> connect
    errorHandler dpy
    checkExtensions dpy
    init dpy >>= evalStateT (eventHandler dpy)

errorHandler :: Connection -> IO ()
errorHandler dpy = void $ forkIO $ do
    forever $ do
        err <- waitForError dpy
        putStrLn $ show err

init :: Connection -> IO HollyState
init dpy = do
    let scrNum = screen $ displayInfo dpy
        s = (!! scrNum) $ roots_Setup $ connectionSetup dpy
        r = root_SCREEN s
    geom <- getGeometry dpy (toDrawable r) >>= getReply'
    redirectSubwindows dpy r RedirectAutomatic
    let rootVisual = root_visual_SCREEN s
    rootF <- findVisualFormat dpy rootVisual

    overlayW <- getOverlayWindow dpy r >>= getReply'
    overlayP <- newResource dpy
    createPicture dpy $! MkCreatePicture
        { pid_CreatePicture = overlayP
        , drawable_CreatePicture = toDrawable overlayW
        , format_CreatePicture = rootF
        , value_CreatePicture = toValueParam
            [(CPSubwindowMode, toValue SubwindowModeIncludeInferiors)]
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
    buffer <- createBuffer dpy (toDrawable overlayW) (w, h) 32 bufferFormat

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
            , destroyNotifyHandler dpy
            , mapNotifyHandler dpy
            , unmapNotifyHandler dpy
            , reparentNotifyHandler dpy
            , circulateNotifyHandler
            , propertyNotifyHandler dpy
            ]
        handler e = do
            mapM_ (\f -> f e) handlers
            mE <- liftIO $ pollForEvent dpy
            case mE of
                Nothing -> return ()
                Just e' -> handler e'
    handler ev

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

-- Event Handlers -------------------------------------------------------

createNotifyHandler :: Connection -> SomeEvent -> StateT HollyState IO ()
createNotifyHandler dpy = guarded $ \ev -> do
    mWin <- liftIO $ getWindow dpy $ window_CreateNotifyEvent ev
    case mWin of
        Nothing -> return ()
        Just win -> withWindows (S.|> win)

configureNotifyHandler :: Connection -> SomeEvent -> StateT HollyState IO ()
configureNotifyHandler dpy = guarded $ \ev -> do
    let wid = window_ConfigureNotifyEvent ev
        abv = above_sibling_ConfigureNotifyEvent ev
    findWindow wid $ discardWindow dpy
    mNew <- liftIO $ getWindow dpy wid
    case mNew of
        Nothing -> return ()
        Just new -> withWindows $ \ws ->
            let (above, below) = S.spanr ((/= abv) . winId) ws
            in below S.>< (new S.<| above)

destroyNotifyHandler :: Connection -> SomeEvent -> StateT HollyState IO ()
destroyNotifyHandler dpy = guarded $ \ev ->
    findWindow (window_DestroyNotifyEvent ev) $ discardWindow dpy

unmapNotifyHandler :: Connection -> SomeEvent -> StateT HollyState IO ()
unmapNotifyHandler dpy = guarded $ \ev ->
    findWindow (window_UnmapNotifyEvent ev) $ discardWindow dpy

mapNotifyHandler :: Connection -> SomeEvent -> StateT HollyState IO ()
mapNotifyHandler dpy = guarded $ \ev -> do
    mWin <- liftIO $ getWindow dpy $ window_MapNotifyEvent ev
    case mWin of
        Nothing -> return ()
        Just win -> withWindows (S.|> win)

reparentNotifyHandler :: Connection -> SomeEvent -> StateT HollyState IO ()
reparentNotifyHandler dpy = guarded $ \ev -> do
    let wid = window_ReparentNotifyEvent ev
    rootWindow <- gets root
    if parent_ReparentNotifyEvent ev == rootWindow
        then liftIO (getWindow dpy wid)
            >>= maybe (return ()) (\win -> withWindows (win S.<|))
        else findWindow wid $ discardWindow dpy

circulateNotifyHandler :: SomeEvent -> StateT HollyState IO ()
circulateNotifyHandler = guarded $ \ev -> do
    withWindows $ \ws -> do
        let wid = window_CirculateNotifyEvent ev
            ws' = S.filter ((/= wid) . winId) ws
        case S.findIndexL ((== wid) . winId) ws of
            Nothing -> ws
            Just wIx ->
                let win = S.index ws wIx
                in case place_CirculateNotifyEvent ev of
                    PlaceOnBottom -> win S.<| ws'
                    PlaceOnTop -> ws' S.|> win

propertyNotifyHandler :: Connection -> SomeEvent -> StateT HollyState IO ()
propertyNotifyHandler dpy = guarded $ \ev -> do
    let wid = window_PropertyNotifyEvent ev
    opacityAtom <- liftIO $ getAtom dpy "_NET_WM_WINDOW_OPACITY" False
    if atom_PropertyNotifyEvent ev == opacityAtom
        then do
            newOpacity <- liftIO $ getWindowOpacity dpy wid
            findWindowIx wid $ \ix -> do
                win <- gets $ flip S.index ix . wins
                liftIO $ damageWholeWindow dpy win
                withWindows $ S.update ix $ win { winOpacity = newOpacity }
        else return ()

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


-- Event Utilities ------------------------------------------------------

guarded :: (Event e, Monad m) => (e -> m ()) -> SomeEvent -> m ()
guarded f ev = let ev' = fromEvent ev in when (isJust ev') $ f $ fromJust ev'

findWindowIx :: WINDOW -> (Int -> StateT HollyState IO ()) -> StateT HollyState IO ()
findWindowIx wid go =
    gets (S.findIndexL ((== wid) . winId) . wins) >>= maybe (return ()) go

findWindow :: WINDOW -> (Win -> StateT HollyState IO ()) -> StateT HollyState IO ()
findWindow wid go = findWindowIx wid
    $ \ix -> gets (flip S.index ix . wins) >>= go

updateWindow :: Connection -> Win -> StateT HollyState IO ()
updateWindow dpy new = findWindowIx (winId new) $ \oldIx -> do
    gets (flip S.index oldIx . wins) >>= freeWindow dpy
    withWindows $ S.update oldIx new

freeWindow :: Connection -> Win -> StateT HollyState IO ()
freeWindow dpy win = liftIO $ do
    Damage.destroy dpy $ winDamage win
    freePicture dpy $ winPicture win

withWindows :: (Seq Win -> Seq Win) -> StateT HollyState IO ()
withWindows f = modify $ \s -> s { wins = f (wins s) }

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

discardWindow :: Connection -> Win -> StateT HollyState IO ()
discardWindow dpy win = do
    extra <- gets extraRepaint
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
    withWindows $ S.filter ((/= winId win) . winId)


-- Utilities ------------------------------------------------------------

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

getReply' :: Receipt a -> IO a
getReply' rcpt = do
    reply <- getReply rcpt
    case reply of
        Left err -> error $ show err
        Right a -> return a

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

class XidLike d => Drawable d where
    toDrawable :: d -> DRAWABLE
    toDrawable = fromXid . toXid

instance Drawable PIXMAP

instance Drawable WINDOW

instance Eq MapState where
    a == b = toValue a == (toValue b :: Integer)

instance Eq PictType where
    a == b = toValue a == (toValue b :: Integer)

instance Eq WindowClass where
    a == b = toValue a == (toValue b :: Integer)
