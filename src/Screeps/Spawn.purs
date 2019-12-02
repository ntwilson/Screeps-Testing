-- | Corresponds to the Screeps API [StructureSpawn](http://support.screeps.com/hc/en-us/articles/205990342-StructureSpawn)
module Screeps.Spawn where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Screeps.Constants (structure_spawn)
import Screeps.FFI (runThisEffFn1, toMaybe, unsafeField)
import Screeps.Structure (unsafeCast)
import Screeps.Types (BodyPartType, Creep, ReturnCode, Spawn, class Structure)

type CreepInfo =
  { name :: String
  , needTime :: Int
  , remainingTime :: Int }

memory :: forall props. Spawn -> { | props }
memory = unsafeField "memory"

name :: Spawn -> String
name = unsafeField "name"

spawning :: Spawn -> Maybe CreepInfo
spawning spawn = toMaybe $ unsafeField "spawning" spawn

foreign import createCreepImpl :: 
  Spawn -> Array BodyPartType -> String -> { memory :: Json, dryRun :: Boolean } -> Effect ReturnCode

createCreep :: 
  forall mem. (EncodeJson mem) =>
  Spawn -> Array BodyPartType -> Maybe String -> { memory :: mem, dryRun :: Boolean } -> Effect ReturnCode
createCreep spawn parts name' { memory: memry, dryRun } = 
  createCreepImpl spawn parts (fromMaybe "" name') { memory: encodeJson memry, dryRun }

recycleCreep :: Spawn -> Creep -> Effect ReturnCode
recycleCreep = runThisEffFn1 "recycleCreep"

renewCreep :: Spawn -> Creep -> Effect ReturnCode
renewCreep = runThisEffFn1 "renewCreep"

toSpawn :: forall a. Structure a => a -> Maybe Spawn
toSpawn = unsafeCast structure_spawn
