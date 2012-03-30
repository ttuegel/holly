{-# LANGUAGE FlexibleInstances #-}
module Holly.Plugins.Stacking where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Sequence ( Seq )
import qualified Data.Sequence as S

import Graphics.XHB

newtype Stacking = Stacking (Seq WINDOW)

class Monad t => StateStacking t where
    liftStack :: StateT Stacking IO a -> t a

instance StateStacking (StateT Stacking IO) where
    liftStack = id

instance StateStacking s => StateStacking (StateT x s) where
    liftStack = lift . liftStack

placeAbove :: StateStacking s => WINDOW -> WINDOW -> s ()
placeAbove w a = liftStack $ modify $ \(Stacking ws) ->
    let (above, below) = S.spanr (/= a) ws
    in Stacking $! below S.>< (w S.<| above)

placeTop :: StateStacking s => WINDOW -> s ()
placeTop w = liftStack $ modify $ \(Stacking ws) -> Stacking $! ws S.|> w

placeBottom :: StateStacking s => WINDOW -> s ()
placeBottom w = liftStack $ modify $ \(Stacking ws) -> Stacking $! w S.<| ws
