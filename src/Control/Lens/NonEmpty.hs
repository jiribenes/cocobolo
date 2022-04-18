-- | This module is copied from Lily: https://github.com/jiribenes/lily
module Control.Lens.NonEmpty
    ( _head
    ) where

import           Control.Lens                   ( Lens' )
import qualified Data.List.NonEmpty            as NE

-- | Equivalent to `_head` but for `NonEmpty`
-- 
-- It's an actual lawful `Lens` instead of a `Traversal`!
_head :: Lens' (NE.NonEmpty a) a
_head f (a NE.:| as) = (NE.:| as) <$> f a
