{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Main where

import Control.Applicative ( (<$>) )
import Control.Concurrent ( forkIO )
import Control.Monad ( forever, unless, void, when )
import Control.Monad.IO.Class
import Data.Foldable ( mapM_ )
import Data.Maybe ( fromJust )
import Data.Sequence ( Seq )
import Data.Traversable ( mapM )
import qualified Data.Sequence as S

import qualified Graphics.XHB.Gen.Composite as Composite
    ( extension
    , queryVersion
    , QueryVersionReply(..)
    )
import Graphics.XHB.Gen.Composite
import Graphics.XHB.Connection.Extension
import Graphics.XHB.Connection.Open hiding ( display )
import qualified Graphics.XHB.Gen.Damage as Damage
import qualified Graphics.XHB.Gen.Render as Render
    ( extension
    , queryVersion
    , QueryVersionReply(..)
    )
import Graphics.XHB.Gen.Render
import qualified Graphics.XHB.Gen.Shape as Shape
    ( extension
    , queryVersion
    , QueryVersionReply(..)
    )
import Graphics.XHB.Gen.Shape hiding ( mask )
import qualified Graphics.XHB.Gen.XFixes as XFixes
    ( extension
    , queryVersion
    , QueryVersionReply(..)
    )
import Graphics.XHB.Gen.XFixes
import Prelude hiding ( init, mapM, mapM_ )

import Holly.Paint
import Holly.Types
import Holly.XHB

-- Main Loop ------------------------------------------------------------

main :: IO ()
main = do
    dpy <- maybe (error "Could not open display!") id <$> connect
    errorHandler dpy
    errorsFatal $ checkExtensions dpy
    initState <- errorsFatal $ init dpy
    evalStateT (errorsFatal $ eventHandler dpy) initState
  where
    errorsFatal :: MonadIO m => EitherT SomeError m a -> m a
    errorsFatal = eitherT (liftIO . error . show) return

errorHandler :: Connection -> IO ()
errorHandler dpy =
    void $ forkIO $ forever $ waitForError dpy >>= putStrLn . show

init :: Connection -> EitherT SomeError IO HollyState
init dpy = do
    let scrNum = screen $ displayInfo dpy
        s = (!! scrNum) $ roots_Setup $ connectionSetup dpy
        r = root_SCREEN s
    geom <- liftIO (getGeometry dpy $ toDrawable r) >>= getReply
    liftIO $ redirectSubwindows dpy r RedirectAutomatic
    let rootVisual = root_visual_SCREEN s
    rootF <- findVisualFormat dpy rootVisual

    overlayW <- liftIO (getOverlayWindow dpy r) >>= getReply
    overlayP <- liftIO $ newResource dpy
    liftIO $ do
        createPicture dpy MkCreatePicture
            { pid_CreatePicture      = overlayP
            , drawable_CreatePicture = toDrawable overlayW
            , format_CreatePicture   = rootF
            , value_CreatePicture    = toValueParam
                [(CPSubwindowMode, toValue SubwindowModeIncludeInferiors)]
            }
        rectangles dpy MkRectangles
            { operation_Rectangles          = SOSet
            , destination_kind_Rectangles   = SKInput
            , ordering_Rectangles           = ClipOrderingUnsorted
            , destination_window_Rectangles = overlayW
            , x_offset_Rectangles           = 0
            , y_offset_Rectangles           = 0
            , rectangles_Rectangles         = []
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
    bufFmt <- findStandardFormat dpy True
    buffer <- liftIO $ createBuffer dpy (toDrawable overlayW) (w, h) 32 bufFmt

    extra <- liftIO $ newResource dpy
    liftIO $ createRegion dpy extra [ MkRECTANGLE 0 0 w h ]

    children <- children_QueryTreeReply
        <$> (liftIO (queryTree dpy r) >>= getReply)

    let hs = HollyState
            { wins           = S.empty
            , display        = dpy
            , scr            = s
            , root           = r
            , rootW          = w
            , rootH          = h
            , rootFormat     = rootF
            , overlayPicture = overlayP
            , overlayWindow  = overlayW
            , bufferPicture  = buffer
            , extraRepaint   = extra
            }

    liftIO $ flip execStateT hs $ do
        ws <- catMaybes <$> mapM (runMaybeT . getWindow) children
        withWindows $ const $ S.fromList ws

checkExtensions :: Connection -> EitherT SomeError IO ()
checkExtensions dpy = do
    compositePresent <- liftIO $ extensionPresent dpy Composite.extension
    unless compositePresent $ error "Composite extension missing!"
    compositeVersion <- liftIO (Composite.queryVersion dpy 0 3) >>= getReply
    let compositeVersionOk = 
               Composite.major_version_QueryVersionReply compositeVersion > 0
            || Composite.minor_version_QueryVersionReply compositeVersion >= 3
    unless compositeVersionOk
        $ error "Composite extension version >= 0.3 required!"

    renderPresent <- liftIO $ extensionPresent dpy Render.extension
    unless renderPresent $ error "Render extension required!"
    renderVersion <- liftIO (Render.queryVersion dpy 0 11) >>= getReply
    let renderVersionOk =
               Render.major_version_QueryVersionReply renderVersion > 0
            || Render.minor_version_QueryVersionReply renderVersion >= 11
    unless renderVersionOk
        $ error "Render extension version >= 0.11 required!"

    fixesPresent <- liftIO $ extensionPresent dpy XFixes.extension
    unless fixesPresent $ error "XFixes extension required!"
    fixesVersion <- liftIO (XFixes.queryVersion dpy 2 0) >>= getReply
    let fixesVersionOk =
            XFixes.major_version_QueryVersionReply fixesVersion >= 2
    unless fixesVersionOk
        $ error "XFixes extension version >= 2 required!"
            
    shapePresent <- liftIO $ extensionPresent dpy Shape.extension
    unless shapePresent $ error "Shape extension required!"
    shapeVersion <- liftIO (Shape.queryVersion dpy) >>= getReply
    let shapeVersionOk =
            (   Shape.major_version_QueryVersionReply shapeVersion == 1
             && Shape.minor_version_QueryVersionReply shapeVersion >= 1
            ) || Shape.major_version_QueryVersionReply shapeVersion > 1
    unless shapeVersionOk
        $ error "Shape extension version >= 1 required!"

    damagePresent <- liftIO $ extensionPresent dpy Damage.extension
    unless damagePresent
        $ error "Damage extension required!"
    damageVersion <- liftIO (Damage.queryVersion dpy 1 1) >>= getReply
    let damageVersionOk =
            (   Damage.major_version_QueryVersionReply damageVersion == 1
             && Damage.minor_version_QueryVersionReply damageVersion >= 1
            ) || Damage.major_version_QueryVersionReply damageVersion > 1
    unless damageVersionOk
        $ error "Damage extension version >= 1.1 required!"

eventHandler :: Connection -> MainLoop ()
eventHandler dpy = forever $ do
    paint
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
            liftIO (pollForEvent dpy) >>= mapM_ handler
    handler ev

-- Event Handlers -------------------------------------------------------

createNotifyHandler, configureNotifyHandler, destroyNotifyHandler,
    unmapNotifyHandler, mapNotifyHandler, reparentNotifyHandler,
    circulateNotifyHandler, propertyNotifyHandler :: EventHandler

createNotifyHandler = guarded $ \ev ->
    void $ lift $ runMaybeT
        $ getWindow (window_CreateNotifyEvent ev) >>= appendWindow
  where appendWindow win = lift $ withWindows (S.|> win)

configureNotifyHandler = guarded $ \ev -> do
    let wid = window_ConfigureNotifyEvent ev
        abv = above_sibling_ConfigureNotifyEvent ev
        arrangeWindows new = lift $ withWindows $ \ws ->
            let (above, below) = S.spanr ((/= abv) . winId) ws
            in below S.>< (new S.<| above)
    lift $ findWindow wid discardWindow
    void $ lift $ runMaybeT $ getWindow wid >>= arrangeWindows

destroyNotifyHandler = guarded $ \ev ->
    lift $ findWindow (window_DestroyNotifyEvent ev) discardWindow

unmapNotifyHandler = guarded $ \ev ->
    lift $ findWindow (window_UnmapNotifyEvent ev) discardWindow

mapNotifyHandler = guarded $ \ev ->
    void $ lift $ runMaybeT $ getWindow (window_MapNotifyEvent ev) >>= mapWindow'
  where mapWindow' win = lift $ withWindows (S.|> win)

reparentNotifyHandler = guarded $ \ev -> do
    let wid = window_ReparentNotifyEvent ev
        prependWindow win = lift $ withWindows (win S.<|)
    rootWindow <- lift $ gets root
    if parent_ReparentNotifyEvent ev == rootWindow
        then void $ lift $ runMaybeT $ getWindow wid >>= prependWindow
        else lift $ findWindow wid $ discardWindow

circulateNotifyHandler = guarded $ \ev -> do
    lift $ withWindows $ \ws -> do
        let wid = window_CirculateNotifyEvent ev
            ws' = S.filter ((/= wid) . winId) ws
            circulate wIx =
                let win = S.index ws wIx
                in case place_CirculateNotifyEvent ev of
                    PlaceOnBottom -> win S.<| ws'
                    PlaceOnTop -> ws' S.|> win
        maybe ws circulate $ S.findIndexL ((== wid) . winId) ws

propertyNotifyHandler = guarded $ \ev -> do
    dpy <- lift $ gets display
    let wid = window_PropertyNotifyEvent ev
    opacityAtom <- getAtom dpy "_NET_WM_WINDOW_OPACITY" False
    if atom_PropertyNotifyEvent ev == opacityAtom
        then do
            newOpacity <- getWindowOpacity dpy wid
            lift $ findWindowIx wid $ \ix -> do
                win <- gets $ flip S.index ix . wins
                damageWholeWindow win
                withWindows $ S.update ix $ win { winOpacity = newOpacity }
        else return ()


-- Event Utilities ------------------------------------------------------

guarded :: (Event e, Monad m) => (e -> m ()) -> SomeEvent -> m ()
guarded f ev = let ev' = fromEvent ev in when (isJust ev') $ f $ fromJust ev'

findWindowIx :: Monad m => WINDOW -> (Int -> StateT HollyState m ()) -> StateT HollyState m ()
findWindowIx wid go =
    gets (S.findIndexL ((== wid) . winId) . wins) >>= maybe (return ()) go

findWindow :: Monad m => WINDOW -> (Win -> StateT HollyState m ()) -> StateT HollyState m ()
findWindow wid go = findWindowIx wid
    $ \ix -> gets (flip S.index ix . wins) >>= go

updateWindow :: MonadIO m => Win -> StateT HollyState m ()
updateWindow new = findWindowIx (winId new) $ \oldIx -> do
    gets (flip S.index oldIx . wins) >>= freeWindow
    withWindows $ S.update oldIx new

freeWindow :: MonadIO m => Win -> StateT HollyState m ()
freeWindow win = do
    dpy <- gets display
    liftIO $ do
        Damage.destroy dpy $ winDamage win
        freePicture dpy $ winPicture win

withWindows :: Monad m => (Seq Win -> Seq Win) -> StateT HollyState m ()
withWindows f = modify $ \s -> s { wins = f (wins s) }

getWindow :: WINDOW -> MaybeT (StateT HollyState IO) Win
getWindow wid = hushT $ do
    dpy <- lift $ gets display
    attrs <- liftIO (getWindowAttributes dpy wid) >>= getReply
    if (inputOnly attrs || not (viewable attrs))
        then left $ toError $ UnknownError ""
        else do
            geom <- liftIO (getGeometry dpy $ toDrawable wid) >>= getReply
            opacity <- getWindowOpacity dpy wid
            fmt <- findVisualFormat dpy
                $ visual_GetWindowAttributesReply attrs
            liftIO $ changeWindowAttributes dpy wid $ toValueParam
                [(CWEventMask, toMask [ EventMaskPropertyChange ])]

            pixmap <- liftIO $ newResource dpy
            dam <- liftIO $ newResource dpy
            pict <- liftIO $ newResource dpy
            liftIO $ do
                nameWindowPixmap dpy wid pixmap
                Damage.create dpy Damage.MkCreate
                    { Damage.damage_Create   = dam
                    , Damage.drawable_Create = toDrawable pixmap
                    , Damage.level_Create    = Damage.ReportLevelNonEmpty
                    }
                createPicture dpy MkCreatePicture
                    { pid_CreatePicture      = pict
                    , drawable_CreatePicture = toDrawable pixmap
                    , format_CreatePicture   = fmt
                    , value_CreatePicture    = toValueParam
                        [( CPSubwindowMode
                         , toValue SubwindowModeIncludeInferiors
                         )]
                    }
                freePixmap dpy pixmap

            let win = Win
                    { winX       = x_GetGeometryReply geom
                    , winY       = y_GetGeometryReply geom
                    , winW       = width_GetGeometryReply geom
                    , winH       = height_GetGeometryReply geom
                    , winB       = border_width_GetGeometryReply geom
                    , winId      = wid
                    , winFormat  = fmt
                    , winOpacity = opacity
                    , winDamage  = dam
                    , winPicture = pict
                    }

            lift $ damageWholeWindow win

            return win
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
