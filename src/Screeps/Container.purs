-- | Corresponds to the Screeps API [StructureContainer](http://support.screeps.com/hc/en-us/articles/208435885-StructureContainer)
module Screeps.Container where

import Data.Maybe (Maybe)

import Screeps.Structure (unsafeCast)
import Screeps.Constants (structure_container)
import Screeps.Types (class Structure, Container)

toContainer :: forall a. Structure a => a -> Maybe Container
toContainer = unsafeCast structure_container
