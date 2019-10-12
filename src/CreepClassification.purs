module CreepClassification 
  ( CreepMemory(..)
  , VocationalCreep(..)
  , UnknownCreepType(..)
  , classifyCreep
  , spawnCreep) where

import Prelude

import CreepRoles (Role(..))
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Argonaut as JSON
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe)
import Effect (Effect)
import Foreign.Object as Object
import Role.Builder (BuilderMemory, Builder)
import Role.Harvester (HarvesterMemory, Harvester)
import Role.Upgrader (UpgraderMemory, Upgrader)
import Screeps.Creep (getAllMemory)
import Screeps.Spawn (createCreep')
import Screeps.Types (BodyPartType, Creep, ReturnCode, Spawn)

data CreepMemory 
  = HarvesterMemory HarvesterMemory
  | BuilderMemory BuilderMemory
  | UpgraderMemory UpgraderMemory

instance encodeCreepMemory :: EncodeJson CreepMemory where
  encodeJson (HarvesterMemory mem) = encodeJson mem
  encodeJson (BuilderMemory mem) = encodeJson mem
  encodeJson (UpgraderMemory mem) = encodeJson mem 

instance decodeCreepMemory :: DecodeJson CreepMemory where
  decodeJson json = lmap (\err -> "Unable to decode creep memory: " <> JSON.stringify json <> ". " <> err) do
    memObj <- JSON.toObject json # note "memory isn't an object?"
    role <- (Object.lookup "role" memObj # note "memory isn't given a role") >>= decodeJson
    case role of 
      HarvesterRole -> pure $ HarvesterMemory { role }
      UpgraderRole -> do
        working <- (Object.lookup "working" memObj >>= JSON.toBoolean) # note "Upgrader memory doesn't have a 'working' field"
        pure $ UpgraderMemory { role, working }
      BuilderRole -> do
        working <- (Object.lookup "working" memObj >>= JSON.toBoolean) # note "Builder memory doesn't have a 'working' field"
        pure $ BuilderMemory { role, working }

data VocationalCreep = Harvester Harvester | Builder Builder | Upgrader Upgrader

newtype UnknownCreepType = UnknownCreepType String

classifyCreep :: Creep -> Effect (Either UnknownCreepType VocationalCreep)
classifyCreep creep = do
  mem <- getAllMemory creep 
  case decodeJson mem of
    Right (HarvesterMemory h) -> pure $ Right $ Harvester { creep, mem: h }
    Right (UpgraderMemory u) -> pure $ Right $ Upgrader { creep, mem: u }
    Right (BuilderMemory b) -> pure $ Right $ Builder { creep, mem: b }
    Left err -> pure $ Left $ UnknownCreepType $ "couldn't classify creep with memory: " <> JSON.stringify mem <> ". " <> err

spawnCreep :: Spawn -> Array BodyPartType -> Maybe String -> CreepMemory -> Effect (Either ReturnCode String)
spawnCreep spawn bodyParts name mem = createCreep' spawn bodyParts name mem
