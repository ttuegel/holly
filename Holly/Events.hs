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

eventHandler :: Connection -> HollyState -> IO ()
eventHandler dpy inState = do
  (xhbEvent, pushXhb) <- newEvent

runHandler :: EventHandler
           -> Connection
           -> SomeEvent
           -> (HollyState, [Repaint])
           -> IO (HollyState, [Repaint])
runHandler evHandler dpy ev (inState, inPaints) =
  fmap (fromMaybe (inState, inPaints)) $ runMaybeT $ do
    (outState, mRepaint) <- evHandler dpy inState ev
    let outPaints = maybe inPaints (: inPaints) mRepaint
    return (outState, outPaints)

-- Event Handlers -------------------------------------------------------

type EventHandler = Tree Win -> SomeEvent -> Maybe (Tree Win, Maybe Repaint)

createHandler :: EventHandler
createHandler tree = withEvent $ \ev ->
  let wid = window_CreateNotifyEvent ev
      win = Win { _wid = wid
                , _x = x_CreateNotifyEvent ev
                , _y = y_CreateNotifyEvent ev
                , _width = width_CreateNotifyEvent ev
                , _height = height_CreateNotifyEvent ev
                , _borderWidth = border_width_CreateNotifyEvent ev
                , _mapped = False }
      pid = parent_CreateNotifyEvent ev
      go node | _wid (rootLabel node) == wid =
                node { subForest = subForest node ++ [win] }
              | otherwise = node { subForest = map go (subForest node) }
      tree' = go tree
   in (tree', Nothing)

configureHandler :: EventHandler
configureHandler tree = withEvent $ \ev ->
  let wid = window_ConfigureNotifyEvent ev
  in fromMaybe (tree, Nothing) $ do
    ix <- S.findIndexL ((== wid) . winId) inWins
    let oldWin = S.index ix inWins
        newWin = oldWin & x .~ x_ConfigureNotifyEvent ev
                        & y .~ y_ConfigureNotifyEvent ev
                        & width .~ width_ConfigureNotifyEvent ev
                        & height .~ height_ConfigureNotifyEvent ev
                        & borderWidth .~ border_width_ConfigureNotifyEvent ev
        abv = above_sibling_ConfigureNotifyEvent ev
        keeping = S.filter ((/= wid) . winId) inWins
        (above, below) = S.spanr ((/= abv) . winId) keeping
    return (below >< (newWin <| above), repaintWindow wid)

destroyHandler :: EventHandler
destroyHandler dpy inWins = withEvent $ \ev ->
  let wid = window_DestroyNotifyEvent ev
  in (S.filter ((/= wid) . winId) inWins, repaintById inWins wid)

repaintById :: Seq Win -> WINDOW -> Maybe Repaint
repaintById wins wid = do
  ix <- S.findIndexL ((== wid) . winId) wins
  return (repaintWindow (S.index ix inWins))

unmapHandler :: EventHandler
unmapHandler dpy inWins = withEvent $ \ev ->
  let wid = window_UnmapNotifyEvent ev
  in fromMaybe (inWins, Nothing) $ do
    ix <- S.findIndexL ((== wid) . winId) inWins
    let outWin = (adjust (& mapped .~ False) ix inWins)
    (S.filter ((/= wid) . _winId) inWins, repaintById inWins wid)

mapHandler :: EventHandler
mapHandler dpy inState = withEvent $ \ev -> do
  win <- getWindow dpy (window_MapNotifyEvent ev)
  return (inState { wins = wins inState |> win }, Nothing)

reparentHandler :: EventHandler
reparentHandler dpy inState = withEvent $ \ev -> do
  outState <- if parent_ReparentNotifyEvent ev == root inState
                then fmap (fromMaybe inState) $ runMaybeT $ do
                  win <- getWindow dpy (window_ReparentNotifyEvent ev)
                  return inState { wins = win <| wins inState }
                else discardWindow dpy (window_ReparentNotifyEvent ev) inState
  return (outState, Nothing)

circulateHandler :: EventHandler
circulateHandler dpy inState = withEvent $ \ev -> do
  let wid = window_CirculateNotifyEvent
      keeping = S.filter ((/= wid) . winId) (wins inState)
      outWins = fromMaybe (wins inState) $ do
        ix <- hoistMaybe $ S.findIndexL ((== wid) . winId) (wins inState)
        let win = S.index ix (wins inState)
        return $ case place_CirculateNotifyEvent ev of
                   PlaceOnBottom -> win <| keeping
                   PlaceOnTop -> keeping |> win
  return (inState { wins = outWins }, Nothing)

propertyHandler :: EventHandler
propertyHandler dpy inState = withEvent $ \ev -> do
  let wid = window_PropertyNotifyEvent ev
  opacityAtom <- getAtom dpy "_NET_WM_WINDOW_OPACITY" False
  if atom_PropertyNotifyEvent ev == opacityAtom
    then do newOpacity <- getWindowOpacity dpy wid
            outWins <- fmap (fromMaybe (wins inState)) $ runMaybeT $ do
              winIx <- hoistMaybe $ S.findIndexL ((== wid) . winId) (wins inState)
              let win = S.index winIx (wins inState)
              liftIO $ damageWholeWindow dpy win
              return (S.update winIx (win { winOpacity = newOpacity }))
            return (inState { wins = outWins }, Nothing)
    else return (inState, Nothing)


-- Event Utilities ------------------------------------------------------

withEvent :: (Event e) => (e -> a) -> SomeEvent -> Maybe a
withEvent f = fmap f . fromEvent
