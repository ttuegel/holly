{-# LINE 1 "Holly/X11.hsc" #-}
module Holly.X11 where
{-# LINE 2 "Holly/X11.hsc" #-}

type Picture = XID
data PictureAttributes = PictureAttributes
    { repeat :: Maybe Repeat
    , alpha :: Maybe (Located Picture)
    , clip  :: Maybe (Located Pixmap)
    , graphicsExposures :: Maybe Bool
    , subwindowMode :: Maybe SubWindowMode
    , polyEdge :: Maybe PolyEdge
    , polyMode :: Maybe PolyMode
    , dither :: Maybe Atom
    , componentAlpha :: Maybe Bool
    }

data PictOp

data Located a = Located
    { x, y :: Position
    , val  :: a
    }

data Repeat = RepeatNone | RepeatNormal | RepeatPad | RepeatReflect

findStandardFormat :: Display -> Int -> IO PictFormat
findVisualFormat :: Display -> Visual -> IO PictFormat
createPicture :: Display -> Drawable -> PictFormat -> PictureAttributes
              -> IO Picture
composite :: Display -> PictOp -> Located Picture -> Maybe (Located Picture)
          -> Located Picture -> (Dimension, Dimension) -> IO ()
fillRectangles :: Display -> PictOp -> Picture -> XRenderColor -> Rectangle
               -> CInt -> IO ()
