{-# LANGUAGE NoImplicitPrelude #-}
module Holly.Events
    ( eventHandler
    ) where

import CustomPrelude
import qualified Data.Sequence as S

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

