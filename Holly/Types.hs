module Holly.Types
    ( HollyState(..)
    , Win(..)
    , module Data.Int
    , module Data.Word
    ) where

import Data.Int
import Data.Sequence ( Seq )
import Data.Word

import Graphics.XHB
import Graphics.XHB.Gen.Damage ( DAMAGE )
import Graphics.XHB.Gen.Render ( PICTFORMAT, PICTURE )
import Graphics.XHB.Gen.XFixes ( REGION )

data HollyState = HollyState
    { wins  :: Seq Win
    , display   :: Connection
    , root  :: WINDOW
    , scr   :: SCREEN
    , rootW :: Word16
    , rootH :: Word16
    , rootFormat :: PICTFORMAT
    , overlayPicture :: PICTURE
    , overlayWindow :: WINDOW
    , bufferPicture :: PICTURE
    , extraRepaint  :: REGION
    }

data Win = Win
    { winId :: WINDOW
    , winX  :: Int16
    , winY  :: Int16
    , winW  :: Word16
    , winH  :: Word16
    , winB  :: Word16
    , winFormat :: PICTFORMAT
    , winOpacity :: Double
    , winDamage :: DAMAGE
    , winPicture    :: PICTURE
    }
  deriving (Eq, Show)
