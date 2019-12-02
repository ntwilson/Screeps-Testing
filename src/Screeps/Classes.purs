module Classes where

import Prelude

import Data.Argonaut (class EncodeJson, Json)
import Effect (Effect)
import Screeps.FFI (unsafeField, unsafeGetFieldEff, unsafeSetFieldEff)
import Screeps.Memory (toJson)
import Screeps.Types (class Structure, Container, Creep, Extension, Flag, Lab, Link, Nuker, PowerSpawn, Room, Ruin, Spawn, Storage, Store, Terminal, Tombstone, Tower)
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

instance containerHasStorage :: HasStorage Container where store = unsafeGetStore
instance creepHasStorage :: HasStorage Creep where store = unsafeGetStore
instance terminalHasStorage :: HasStorage Terminal where store = unsafeGetStore
instance towerHasStorage :: HasStorage Tower where store = unsafeGetStore
instance tombstoneHasStorage :: HasStorage Tombstone where store = unsafeGetStore
instance extensionHasStorage :: HasStorage Extension where store = unsafeGetStore
instance labHasStorage :: HasStorage Lab where store = unsafeGetStore
instance nukerHasStorage :: HasStorage Nuker where store = unsafeGetStore
instance ruinHasStorage :: HasStorage Ruin where store = unsafeGetStore
instance linkHasStorage :: HasStorage Link where store = unsafeGetStore
instance powerSpawnHasStorage :: HasStorage PowerSpawn where store = unsafeGetStore
instance spawnHasStorage :: HasStorage Spawn where store = unsafeGetStore
instance storageHasStorage :: HasStorage Storage where store = unsafeGetStore

-- | note this makes the assumption that _every_ kind of store can hold
-- | energy.  See https://docs.screeps.com/api/#Store.
energy :: forall a. HasStorage a => a -> Int 
energy x = store x # getEnergy 

-- | note this makes the assumption that _every_ kind of store can hold
-- | energy.  See https://docs.screeps.com/api/#Store.
energyCapacity :: forall a. HasStorage a => a -> Int
energyCapacity x = store x # getEnergyCapacity


class HasMemory a where
  getMemory :: a -> Effect Json
  setMemory :: forall b. EncodeJson b => a -> b -> Effect Unit

unsafeGetMemory :: forall a. a -> Effect Json
unsafeGetMemory x = x # unsafeGetFieldEff "memory"

unsafeSetMemory :: forall a b. EncodeJson b => a -> b -> Effect Unit
unsafeSetMemory x val = unsafeSetFieldEff "memory" x (toJson val)

instance creepHasMemory :: HasMemory Creep where
  getMemory = unsafeGetMemory
  setMemory = unsafeSetMemory

instance spawnHasMemory :: HasMemory Spawn where
  getMemory = unsafeGetMemory
  setMemory = unsafeSetMemory

instance flagHasMemory :: HasMemory Flag where
  getMemory = unsafeGetMemory
  setMemory = unsafeSetMemory

instance roomHasMemory :: HasMemory Room where
  getMemory = unsafeGetMemory
  setMemory = unsafeSetMemory
  