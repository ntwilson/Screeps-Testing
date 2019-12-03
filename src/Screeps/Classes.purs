module Classes where

import Prelude

import Data.Argonaut (class EncodeJson, Json)
import Effect (Effect)
import Screeps.FFI (unsafeField, unsafeGetFieldEff, unsafeSetFieldEff)
import Screeps.Memory (toJson)
import Screeps.Types (class Structure, Container, Creep, Extension, Flag, Lab, Link, Nuker, PowerSpawn, Room, Ruin, Source, Spawn, Storage, Store, Terminal, Tombstone, Tower)
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


class HasStore a where
  store :: a -> Store

unsafeGetStore :: forall a. a -> Store
unsafeGetStore = unsafeField "store"

instance containerHasStore :: HasStore Container where store = unsafeGetStore
instance creepHasStore :: HasStore Creep where store = unsafeGetStore
instance terminalHasStore :: HasStore Terminal where store = unsafeGetStore
instance towerHasStore :: HasStore Tower where store = unsafeGetStore
instance tombstoneHasStore :: HasStore Tombstone where store = unsafeGetStore
instance extensionHasStore :: HasStore Extension where store = unsafeGetStore
instance labHasStore :: HasStore Lab where store = unsafeGetStore
instance nukerHasStore :: HasStore Nuker where store = unsafeGetStore
instance ruinHasStore :: HasStore Ruin where store = unsafeGetStore
instance linkHasStore :: HasStore Link where store = unsafeGetStore
instance powerSpawnHasStore :: HasStore PowerSpawn where store = unsafeGetStore
instance spawnHasStore :: HasStore Spawn where store = unsafeGetStore
instance storageHasStore :: HasStore Storage where store = unsafeGetStore

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
  

class HasEnergy a where
  energy :: a -> Int
  energyCapacity :: a -> Int

instance sourceHasEnergy :: HasEnergy Source where
  energy = unsafeField "energy"
  energyCapacity = unsafeField "energyCapacity"

else instance storeHasEnergy :: HasStore a => HasEnergy a where
  energy x = store x # getEnergy 
  energyCapacity x = store x # getEnergyCapacity
