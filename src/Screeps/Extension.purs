-- | Corresponds to the Screeps API [StructureExtension](http://support.screeps.com/hc/en-us/articles/207711949-StructureExtension)
module Screeps.Extension where

import Data.Maybe (Maybe)

import Screeps.Constants (structure_extension)
import Screeps.FFI (unsafeField)
import Screeps.Structure (unsafeCast)
import Screeps.Types (Extension, class Structure)

toExtension :: forall a. Structure a => a -> Maybe Extension
toExtension = unsafeCast structure_extension
