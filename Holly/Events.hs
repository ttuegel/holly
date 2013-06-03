{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Holly.Events
    ( getWindow
    , eventHandler
    , withWindows
    ) where

import CustomPrelude
import qualified Data.Sequence as S
import Graphics.XHB.Gen.Composite
import qualified Graphics.XHB.Gen.Damage as Damage
import Graphics.XHB.Gen.Render
import Graphics.XHB.Gen.XFixes

import Holly.Paint
import Holly.Types
import Holly.XHB

eventHandler :: Connection -> MainLoop ()
eventHandler dpy = forever $ do
    paint dpy
    ev <- liftIO $ waitForEvent dpy
    let handlers = map ($ dpy)
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
    circulateNotifyHandler, propertyNotifyHandler :: Connection -> EventHandler

createNotifyHandler dpy = guarded $ \ev ->
    void $ lift $ runMaybeT
        $ getWindow dpy (window_CreateNotifyEvent ev) >>= appendWindow
  where appendWindow win = lift $ withWindows (|> win)

configureNotifyHandler dpy = guarded $ \ev -> do
    let wid = window_ConfigureNotifyEvent ev
        abv = above_sibling_ConfigureNotifyEvent ev
        arrangeWindows new = lift $ withWindows $ \ws ->
            let (above, below) = S.spanr ((/= abv) . winId) ws
            in below >< (new <| above)
    lift $ findWindow wid (discardWindow dpy)
    void $ lift $ runMaybeT $ getWindow dpy wid >>= arrangeWindows

destroyNotifyHandler dpy = guarded $ \ev ->
    lift $ findWindow (window_DestroyNotifyEvent ev) (discardWindow dpy)

unmapNotifyHandler dpy = guarded $ \ev ->
    lift $ findWindow (window_UnmapNotifyEvent ev) (discardWindow dpy)

mapNotifyHandler dpy = guarded $ \ev ->
    void $ lift $ runMaybeT $ getWindow dpy (window_MapNotifyEvent ev) >>= mapWindow'
  where mapWindow' win = lift $ withWindows (|> win)

reparentNotifyHandler dpy = guarded $ \ev -> do
    let wid = window_ReparentNotifyEvent ev
        prependWindow win = lift $ withWindows (win <|)
    rootWindow <- lift $ gets root
    if parent_ReparentNotifyEvent ev == rootWindow
        then void $ lift $ runMaybeT $ getWindow dpy wid >>= prependWindow
        else lift $ findWindow wid $ discardWindow dpy

circulateNotifyHandler _ = guarded $ \ev -> do
    lift $ withWindows $ \ws -> do
        let wid = window_CirculateNotifyEvent ev
            ws' = S.filter ((/= wid) . winId) ws
            circulate wIx =
                let win = S.index ws wIx
                in case place_CirculateNotifyEvent ev of
                    PlaceOnBottom -> win <| ws'
                    PlaceOnTop -> ws' |> win
        maybe ws circulate $ S.findIndexL ((== wid) . winId) ws

propertyNotifyHandler dpy = guarded $ \ev -> do
    let wid = window_PropertyNotifyEvent ev
    opacityAtom <- getAtom dpy "_NET_WM_WINDOW_OPACITY" False
    if atom_PropertyNotifyEvent ev == opacityAtom
        then do
            newOpacity <- getWindowOpacity dpy wid
            lift $ findWindowIx wid $ \ix -> do
                win <- gets $ flip S.index ix . wins
                damageWholeWindow dpy win
                withWindows $ S.update ix $ win { winOpacity = newOpacity }
        else return ()


-- Event Utilities ------------------------------------------------------

guarded :: (Event e, Monad m) => (e -> m ()) -> SomeEvent -> m ()
guarded f ev = mapM_ f $ fromEvent ev

findWindowIx :: Monad m => WINDOW -> (Int -> StateT HollyState m ()) -> StateT HollyState m ()
findWindowIx wid go =
    gets (S.findIndexL ((== wid) . winId) . wins) >>= maybe (return ()) go

findWindow :: Monad m => WINDOW -> (Win -> StateT HollyState m ()) -> StateT HollyState m ()
findWindow wid go = findWindowIx wid
    $ \ix -> gets (flip S.index ix . wins) >>= go

freeWindow :: MonadIO m => Connection -> Win -> StateT HollyState m ()
freeWindow dpy win = do
    liftIO $ do
        Damage.destroy dpy $ winDamage win
        freePicture dpy $ winPicture win

withWindows :: Monad m => (Seq Win -> Seq Win) -> StateT HollyState m ()
withWindows f = modify $ \s -> s { wins = f (wins s) }

getWindow :: Connection -> WINDOW -> MaybeT (StateT HollyState IO) Win
getWindow dpy wid = hushT $ do
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

            lift $ damageWholeWindow dpy win

            return win
  where
    viewable = (== MapStateViewable) . map_state_GetWindowAttributesReply
    inputOnly = (== WindowClassInputOnly) . class_GetWindowAttributesReply

damageWholeWindow :: Connection -> Win -> StateT HollyState IO ()
damageWholeWindow dpy win = do
    let x = winX win
        y = winY win
        w = winW win
        h = winH win
        bI = fromIntegral $ winB win
        bW = winB win
    extra <- gets extraRepaint
    liftIO $ do
        region <- newResource dpy
        createRegion dpy region
            [ MkRECTANGLE (x - bI) (y - bI) (w + bW + bW) (h + bW + bW) ]
        unionRegion dpy $! MkUnionRegion region extra extra
        destroyRegion dpy region

discardWindow :: Connection -> Win -> StateT HollyState IO ()
discardWindow dpy win = do
    damageWholeWindow dpy win
    freeWindow dpy win
    withWindows $ S.filter ((/= winId win) . winId)
