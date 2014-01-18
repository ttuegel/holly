{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TemplateHaskell #-}
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
import Data.Lens
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

data Repaint = RepaintRegion REGION
             | RepaintRect RECTANGLE
  deriving (Eq, Ord, Read, Show)
