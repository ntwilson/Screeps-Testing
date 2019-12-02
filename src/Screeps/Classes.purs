module Classes where

import Prelude

import Screeps.FFI (unsafeField)
import Screeps.Types (class Structure, Container, Creep, Extension, Lab, Link, Nuker, PowerSpawn, Ruin, Spawn, Store, Terminal, Tombstone, Tower)
import Store (getEnergy, getEnergyCapacity)

class HasHealth a where
  hits :: a -> Int
  hitsMax :: a -> Int

unsafeGetHits :: forall a. a -> Int
unsafeGetHits = unsafeField "hits"

unsafeGetHitsMax :: forall a. a -> Int
unsafeGetHitsMax = unsafeField "hitsMax"

instance creepHasHealth :: HasHealth Creep where
  hits = unsafeGetHits
  hitsMax = unsafeGetHitsMax

else instance structureHasHealth :: Structure a => HasHealth a where
  hits = unsafeGetHits
  hitsMax = unsafeGetHitsMax


class HasStorage a where
  store :: a -> Store

unsafeGetStore :: forall a. a -> Store
unsafeGetStore = unsafeField "store"

instance containerStorage :: HasStorage Container where store = unsafeGetStore
instance creepStorage :: HasStorage Creep where store = unsafeGetStore
instance terminalStorage :: HasStorage Terminal where store = unsafeGetStore
instance towerStorage :: HasStorage Tower where store = unsafeGetStore
instance tombstoneStorage :: HasStorage Tombstone where store = unsafeGetStore
instance extensionStorage :: HasStorage Extension where store = unsafeGetStore
instance labStorage :: HasStorage Lab where store = unsafeGetStore
instance nukerStorage :: HasStorage Nuker where store = unsafeGetStore
instance ruinStorage :: HasStorage Ruin where store = unsafeGetStore
instance linkStorage :: HasStorage Link where store = unsafeGetStore
instance powerSpawnStorage :: HasStorage PowerSpawn where store = unsafeGetStore
instance spawnStorage :: HasStorage Spawn where store = unsafeGetStore

-- | note this makes the assumption that _every_ kind of store can hold
-- | energy.  See https://docs.screeps.com/api/#Store.
energy :: forall a. HasStorage a => a -> Int 
energy x = store x # getEnergy 

-- | note this makes the assumption that _every_ kind of store can hold
-- | energy.  See https://docs.screeps.com/api/#Store.
energyCapacity :: forall a. HasStorage a => a -> Int
energyCapacity x = store x # getEnergyCapacity


