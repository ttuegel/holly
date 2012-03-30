{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Applicative ( (<$>) )
import Control.Concurrent ( forkIO )
import Control.Monad ( forever, unless, void, when )
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Maybe ( catMaybes, fromJust, isJust )
import Data.Sequence ( Seq )
import Data.Traversable ( mapM )
import qualified Data.Sequence as S
import Graphics.XHB
import qualified Graphics.XHB.Gen.Composite as Composite
    ( extension, queryVersion, QueryVersionReply(..) )
import Graphics.XHB.Gen.Composite
import Graphics.XHB.Connection.Extension
import Graphics.XHB.Connection.Open hiding ( display )
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

import Holly.Drawable
import Holly.Missing
import Holly.Paint
import Holly.Types

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

    let w = width_GetGeometryReply geom
        h = height_GetGeometryReply geom
    bufferFormat <- findStandardFormat dpy True
    buffer <- createBuffer dpy (toDrawable overlayW) (w, h) 32 bufferFormat

    extra <- newResource dpy
    createRegion dpy extra [ MkRECTANGLE 0 0 w h ]

    children <- children_QueryTreeReply <$> (queryTree dpy r >>= getReply')

    let hs = HollyState
            { wins = S.empty
            , display = dpy
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

    flip execStateT hs $ do
        ws <- catMaybes <$> mapM getWindow children
        withWindows $ const $ S.fromList ws

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
            [ createNotifyHandler
            , configureNotifyHandler
            , destroyNotifyHandler
            , mapNotifyHandler
            , unmapNotifyHandler
            , reparentNotifyHandler
            , circulateNotifyHandler
            , propertyNotifyHandler
            ]
        handler e = do
            mapM_ (\f -> f e) handlers
            mE <- liftIO $ pollForEvent dpy
            case mE of
                Nothing -> return ()
                Just e' -> handler e'
    handler ev

-- Event Handlers -------------------------------------------------------

createNotifyHandler ::SomeEvent -> StateT HollyState IO ()
createNotifyHandler = guarded $ \ev -> do
    mWin <- getWindow $ window_CreateNotifyEvent ev
    case mWin of
        Nothing -> return ()
        Just win -> withWindows (S.|> win)

configureNotifyHandler :: SomeEvent -> StateT HollyState IO ()
configureNotifyHandler = guarded $ \ev -> do
    let wid = window_ConfigureNotifyEvent ev
        abv = above_sibling_ConfigureNotifyEvent ev
    findWindow wid $ discardWindow
    mNew <- getWindow wid
    case mNew of
        Nothing -> return ()
        Just new -> withWindows $ \ws ->
            let (above, below) = S.spanr ((/= abv) . winId) ws
            in below S.>< (new S.<| above)

destroyNotifyHandler :: SomeEvent -> StateT HollyState IO ()
destroyNotifyHandler = guarded $ \ev ->
    findWindow (window_DestroyNotifyEvent ev) $ discardWindow

unmapNotifyHandler :: SomeEvent -> StateT HollyState IO ()
unmapNotifyHandler = guarded $ \ev ->
    findWindow (window_UnmapNotifyEvent ev) $ discardWindow

mapNotifyHandler :: SomeEvent -> StateT HollyState IO ()
mapNotifyHandler = guarded $ \ev -> do
    mWin <- getWindow $ window_MapNotifyEvent ev
    case mWin of
        Nothing -> return ()
        Just win -> withWindows (S.|> win)

reparentNotifyHandler :: SomeEvent -> StateT HollyState IO ()
reparentNotifyHandler = guarded $ \ev -> do
    let wid = window_ReparentNotifyEvent ev
    rootWindow <- gets root
    if parent_ReparentNotifyEvent ev == rootWindow
        then getWindow wid
            >>= maybe (return ()) (\win -> withWindows (win S.<|))
        else findWindow wid $ discardWindow

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

propertyNotifyHandler :: SomeEvent -> StateT HollyState IO ()
propertyNotifyHandler = guarded $ \ev -> do
    dpy <- gets display
    let wid = window_PropertyNotifyEvent ev
    opacityAtom <- liftIO $ getAtom dpy "_NET_WM_WINDOW_OPACITY" False
    if atom_PropertyNotifyEvent ev == opacityAtom
        then do
            newOpacity <- liftIO $ getWindowOpacity dpy wid
            findWindowIx wid $ \ix -> do
                win <- gets $ flip S.index ix . wins
                damageWholeWindow win
                withWindows $ S.update ix $ win { winOpacity = newOpacity }
        else return ()


-- Event Utilities ------------------------------------------------------

guarded :: (Event e, Monad m) => (e -> m ()) -> SomeEvent -> m ()
guarded f ev = let ev' = fromEvent ev in when (isJust ev') $ f $ fromJust ev'

findWindowIx :: WINDOW -> (Int -> StateT HollyState IO ()) -> StateT HollyState IO ()
findWindowIx wid go =
    gets (S.findIndexL ((== wid) . winId) . wins) >>= maybe (return ()) go

findWindow :: WINDOW -> (Win -> StateT HollyState IO ()) -> StateT HollyState IO ()
findWindow wid go = findWindowIx wid
    $ \ix -> gets (flip S.index ix . wins) >>= go

updateWindow :: Win -> StateT HollyState IO ()
updateWindow new = findWindowIx (winId new) $ \oldIx -> do
    gets (flip S.index oldIx . wins) >>= freeWindow
    withWindows $ S.update oldIx new

freeWindow :: Win -> StateT HollyState IO ()
freeWindow win = do
    dpy <- gets display
    liftIO $ do
        Damage.destroy dpy $ winDamage win
        freePicture dpy $ winPicture win

withWindows :: (Seq Win -> Seq Win) -> StateT HollyState IO ()
withWindows f = modify $ \s -> s { wins = f (wins s) }

getWindow :: WINDOW -> StateT HollyState IO (Maybe Win)
getWindow wid = do
    dpy <- gets display
    attrs <- liftIO $ getWindowAttributes dpy wid >>= getReply'
    if (inputOnly attrs || not (viewable attrs))
        then return Nothing
        else do
            win <- liftIO $ do
                        geom <- getGeometry dpy (toDrawable wid) >>= getReply'
                        opacity <- getWindowOpacity dpy wid
                        fmt <- findVisualFormat dpy $ visual_GetWindowAttributesReply attrs
                        changeWindowAttributes dpy wid $! toValueParam
                            [(CWEventMask, toMask [ EventMaskPropertyChange ])]
                        pixmap <- newResource dpy
                        nameWindowPixmap dpy wid pixmap
                        dam <- newResource dpy
                        Damage.create dpy $! Damage.MkCreate
                            { Damage.damage_Create = dam
                            , Damage.drawable_Create = toDrawable pixmap
                            , Damage.level_Create = Damage.ReportLevelNonEmpty
                            }
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
                        return $! Win
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
            damageWholeWindow win
            return $! Just $! win
  where
    viewable = (== MapStateViewable) . map_state_GetWindowAttributesReply
    inputOnly = (== WindowClassInputOnly) . class_GetWindowAttributesReply

damageWholeWindow :: Win -> StateT HollyState IO ()
damageWholeWindow win = do
    let x = winX win
        y = winY win
        w = winW win
        h = winH win
        bI = fromIntegral $ winB win
        bW = winB win
    extra <- gets extraRepaint
    dpy <- gets display
    liftIO $ do
        region <- newResource dpy
        createRegion dpy region
            [ MkRECTANGLE (x - bI) (y - bI) (w + bW + bW) (h + bW + bW) ]
        unionRegion dpy $! MkUnionRegion region extra extra
        destroyRegion dpy region

discardWindow :: Win -> StateT HollyState IO ()
discardWindow win = do
    damageWholeWindow win
    freeWindow win
    withWindows $ S.filter ((/= winId win) . winId)
