-- | Corresponds to the Screeps API [Game](http://support.screeps.com/hc/en-us/articles/203016382-Game)
module Screeps.Game where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe)
import Foreign.Object (Object)

import Screeps.Types (ConstructionSite, Creep, Flag, GameGlobal, Id, Market, Room, Spawn, class Structure, WorldMap)
import Screeps.FFI (runThisEffFn0, runThisEffFn1, runThisEffFn2, runThisFn1, toMaybe, unsafeField)

foreign import getGameGlobal :: Effect GameGlobal

type Gcl =
  { level :: Int
  , progress :: Int
  , progressTotal :: Int }

type Cpu =
  { limit :: Int
  , tickLimit :: Int
  , bucket :: Int }

constructionSites :: GameGlobal -> Object ConstructionSite
constructionSites = unsafeField "constructionSites"

cpu :: GameGlobal -> Cpu
cpu = unsafeField "cpu"

creeps :: GameGlobal -> Object Creep
creeps = unsafeField "creeps"

flags :: GameGlobal -> Object Flag
flags = unsafeField "flags"

gcl :: GameGlobal -> Gcl
gcl = unsafeField "gcl"

map :: GameGlobal -> WorldMap
map = unsafeField "map"

market :: GameGlobal -> Market
market = unsafeField "market"

rooms :: GameGlobal -> Object Room
rooms = unsafeField "rooms"

spawns :: GameGlobal -> Object Spawn
spawns = unsafeField "spawns"

structures :: GameGlobal -> Object (forall a. Structure a => a)
structures = unsafeField "structures"

time :: GameGlobal -> Int
time = unsafeField "time"

getUsed :: GameGlobal -> Effect Number
getUsed game = runThisEffFn0 "getUsed" (cpu game)

getObjectById :: forall a. GameGlobal -> Id a -> Maybe a
getObjectById game id = toMaybe $ runThisFn1 "getObjectById" game id

notify :: GameGlobal -> String -> Effect Unit
notify game msg = runThisEffFn1 "notify" game msg

notify' :: GameGlobal -> String -> Int -> Effect Unit
notify' game msg groupInterval = runThisEffFn2 "notify" game msg groupInterval
