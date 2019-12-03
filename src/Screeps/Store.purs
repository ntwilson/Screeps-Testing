module Store where

import Prelude

import Data.Maybe (Maybe, fromJust)
import Partial.Unsafe (unsafePartial)
import Screeps (resource_energy)
import Screeps.FFI (toMaybe, unsafeField)
import Screeps.Types (ResourceType(..), Store)

-- | returns Nothing if that resource type is not valid for that
-- | type of Store
getCapacity :: Store -> ResourceType -> Maybe Int
getCapacity store (ResourceType resource) = 
  let capacityFn = store # unsafeField "getCapacity" 
  in capacityFn resource # toMaybe

-- | returns Nothing if that resource type is not valid for that
-- | type of Store
getUsedCapacity :: Store -> ResourceType -> Maybe Int
getUsedCapacity store (ResourceType resource) = 
  let capacityFn = store # unsafeField "getUsedCapacity"
  in capacityFn resource # toMaybe

-- | returns Nothing if that resource type is not valid for that
-- | type of Store
getFreeCapacity :: Store -> ResourceType -> Maybe Int
getFreeCapacity store resource = do
  totalCapacity <- store `getCapacity` resource
  usedCapacity <- store `getUsedCapacity` resource
  pure $ totalCapacity - usedCapacity

-- | note this makes the assumption that _every_ kind of store can hold
-- | energy.  See https://docs.screeps.com/api/#Store.
getEnergy :: Store -> Int 
getEnergy store = unsafePartial $ fromJust $ store `getUsedCapacity` resource_energy 

-- | note this makes the assumption that _every_ kind of store can hold
-- | energy.  See https://docs.screeps.com/api/#Store.
getEnergyCapacity :: Store -> Int
getEnergyCapacity store = unsafePartial $ fromJust $ store `getCapacity` resource_energy 
