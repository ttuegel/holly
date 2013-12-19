module Holly.Types
    ( HollyState(..)
    , Win(..)
    , module Data.Int
    , module Data.Word
    ) where

import Data.Int
import Data.Sequence ( Seq )
import Data.Word

import Holly.X11

data HollyState = HollyState
    { wins  :: Seq Win
    , display   :: Display
    , root  :: Window
    , scr   :: Screen
    , rootW :: Word16
    , rootH :: Word16
    , rootFormat :: PictFormat
    , overlayPicture :: Picture
    , overlayWindow :: Window
    , bufferPicture :: Picture
    , extraRepaint  :: Region
    }

data Win = Win
    { winId :: Window
    , winX  :: Int16
    , winY  :: Int16
    , winW  :: Word16
    , winH  :: Word16
    , winB  :: Word16
    , winFormat :: PictFormat
    , winOpacity :: Double
    , winDamage :: Damage
    , winPicture    :: Picture
    }
