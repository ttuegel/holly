module Holly.Window where

import Data.Lens
import Data.Tree

import Graphics.XHB

data Win = Win { _wid :: WINDOW
               , _x :: Int16
               , _y :: Int16
               , _width :: Word16
               , _height :: Word16
               , _borderWidth :: Word16
               , _mapped :: Bool }
  deriving (Data, Eq, Ord, Read, Show, Typeable)
makeLenses ''Win

getWinTree :: WINDOW -> IO (Tree Win)
getWinTree = unfoldTreeM $ \wid -> do
  geom <- getGeometry dpy (toDrawable wid) >>= getReply
  mapState <- map_state_GetWindowAttributesReply
                <$> (getWindowAttributes dpy wid >>= getReply)
  let win = Win { _wid = wid
                , _x = x_GetGeometryReply geom
                , _y = y_GetGeometryReply geom
                , _width = width_GetGeometryReply geom
                , _height = height_GetGeometryReply geom
                , _borderWidth = border_width_GetGeometryReply geom
                , _mapped = case mapState of
                              MapStateUnmapped -> False
                              MapStateUnviewable -> True
                              MapStateViewable -> True }
  children <- children_QueryTreeReply <$> (queryTree dpy wid >>= getReply)
  return (win, children)
