{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Holly.Types
    ( EventHandler
    , HollyState(..)
    , MainLoop
    , Win(..)
    , damageWholeWindow
    , discardWindow
    , findWindow
    , findWindowIx
    , getWindow
    , withWindows
    , module Data.Int
    , module Data.Sequence
    , module Data.Word
    , module Control.Error
    , module Control.Monad.Trans.Class
    , module Control.Monad.Trans.State
    ) where

import Control.Error
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
    ( StateT(..)
    , evalStateT
    , execStateT
    , get
    , gets
    , modify
    , put
    )
import CustomPrelude
import Data.Int
import Data.Sequence ( Seq, (<|), (|>), (><) )
import qualified Data.Sequence as S
import Data.Word
import Graphics.XHB.Gen.Composite
import Graphics.XHB.Gen.Damage ( DAMAGE )
import qualified Graphics.XHB.Gen.Damage as Damage
import Graphics.XHB.Gen.Render
import Graphics.XHB.Gen.XFixes

import Holly.XHB

data HollyState = HollyState
    { wins           :: Seq Win
    , root           :: WINDOW
    , scr            :: SCREEN
    , rootW          :: Word16
    , rootH          :: Word16
    , rootFormat     :: PICTFORMAT
    , overlayPicture :: PICTURE
    , overlayWindow  :: WINDOW
    , bufferPicture  :: PICTURE
    , extraRepaint   :: REGION
    }

data Win = Win
    { winId      :: WINDOW
    , winX       :: Int16
    , winY       :: Int16
    , winW       :: Word16
    , winH       :: Word16
    , winB       :: Word16
    , winFormat  :: PICTFORMAT
    , winOpacity :: Double
    , winDamage  :: DAMAGE
    , winPicture :: PICTURE
    }
  deriving (Eq, Show)

type MainLoop a = EitherT SomeError (StateT HollyState IO) a
type EventHandler = SomeEvent -> MainLoop ()


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
