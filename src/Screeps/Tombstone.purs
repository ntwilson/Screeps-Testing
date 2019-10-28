module Tombstone where

import Prelude

import Screeps.FFI (unsafeField)
import Screeps.Types (Store, Tombstone)

store :: Tombstone -> Store
store tombstone = tombstone # unsafeField "store"
