module Holly.Drawable
    ( Drawable(..)
    ) where

import Graphics.XHB

class XidLike d => Drawable d where
    toDrawable :: d -> DRAWABLE
    toDrawable = fromXid . toXid

instance Drawable PIXMAP

instance Drawable WINDOW

