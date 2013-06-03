{-# LANGUAGE BangPatterns, NoImplicitPrelude, OverloadedStrings #-}
module Main where

import Control.Concurrent.Async
import Control.Monad.IO.Class
import CustomPrelude hiding ( init )
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

import Holly.Events
import Holly.Types
import Holly.XHB

-- Main Loop ------------------------------------------------------------

main :: IO ()
main = do
    dpy <- maybe (error "Could not open display!") id <$> connect
    errThread <- async $ errorHandler dpy
    errorsFatal $ checkExtensions dpy
    initState <- errorsFatal $ init dpy
    mainThread <- async $ evalStateT (errorsFatal $ eventHandler dpy) initState
    void $ waitAny [ errThread, mainThread ]
  where
    errorsFatal :: MonadIO m => EitherT SomeError m a -> m a
    errorsFatal = eitherT (liftIO . error . show) return

errorHandler :: Connection -> IO ()
errorHandler dpy =
    void $ forever $ waitForError dpy >>= putStrLn . show

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
        ws <- catMaybes <$> mapM (runMaybeT . getWindow dpy) children
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

