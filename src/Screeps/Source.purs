-- | Corresponds to the Screeps API [Source](http://support.screeps.com/hc/en-us/articles/203079211-Source)
module Screeps.Source where

import Screeps.Types (Id, Source)
import Screeps.FFI (unsafeField)

id :: Source -> Id Source
id = unsafeField "id"

ticksToRegeneration :: Source -> Int
ticksToRegeneration = unsafeField "ticksToRegeneration"
