-- | Corresponds to the Screeps API [Room](http://support.screeps.com/hc/en-us/articles/203079011-Room)
module Screeps.Room where

import Prelude

import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Nothing))
import Effect (Effect)
import Screeps.FFI (runThisEffFn1, runThisEffFn2, runThisEffFn3, runThisEffFn4, runThisEffFn5, runThisFn1, runThisFn2, runThisFn3, selectMaybes, toMaybe, unsafeField)
import Screeps.Types (Color, Controller, FilterFn, FindType, LookType, Mode, Path, ReturnCode, Room, RoomPosition, Storage, StructureType, TargetPosition(..), Terminal)

foreign import data RoomGlobal :: Type
foreign import getRoomGlobal :: Effect RoomGlobal

-- TODO: costCallback option
type PathOptions o =
  { ignoreCreeps :: Maybe Boolean
  , ignoreDestructibleStructures :: Maybe Boolean
  , ignoreRoads :: Maybe Boolean
  , ignore :: Maybe (Array RoomPosition)
  , avoid :: Maybe (Array RoomPosition)
  , maxOps :: Maybe Int
  , heuristicWeight :: Maybe Number
  , serialize :: Maybe Boolean
  , maxRooms :: Maybe Int
  | o }

pathOpts :: PathOptions ()
pathOpts =
  { ignoreCreeps: Nothing
  , ignoreDestructibleStructures: Nothing
  , ignoreRoads: Nothing
  , ignore: Nothing
  , avoid: Nothing
  , maxOps: Nothing
  , heuristicWeight: Nothing
  , serialize: Nothing
  , maxRooms: Nothing }

controller :: Room -> Maybe Controller
controller room = toMaybe $ unsafeField "controller" room

energyAvailable :: Room -> Int
energyAvailable = unsafeField "energyAvailable"

energyCapacityAvailable :: Room -> Int
energyCapacityAvailable = unsafeField "energyCapacityAvailable"

mode :: Room -> Mode
mode = unsafeField "mode"

name :: Room -> String
name = unsafeField "name"

storage :: Room -> Maybe Storage
storage room = toMaybe $ unsafeField "storage" room

terminal :: Room -> Maybe Terminal
terminal room = toMaybe $ unsafeField "terminal" room

serializePath :: RoomGlobal -> Path -> String
serializePath = runThisFn1 "serializePath"

deserializePath :: RoomGlobal -> String -> Path
deserializePath = runThisFn1 "deserializePath"

createConstructionSite :: Room -> TargetPosition -> StructureType -> Effect ReturnCode
createConstructionSite room (TargetPt x' y') strucType = runThisEffFn3 "createConstructionSite" room x' y' strucType
createConstructionSite room (TargetPos pos) strucType = runThisEffFn2 "createConstructionSite" room pos strucType

createFlag :: Room -> TargetPosition -> Effect ReturnCode
createFlag room (TargetPt x' y') = runThisEffFn2 "createFlag" room x' y'
createFlag room (TargetPos pos) = runThisEffFn1 "createFlag" room pos

createFlagWithName :: Room -> TargetPosition -> String -> Effect ReturnCode
createFlagWithName room (TargetPt x' y') name' = runThisEffFn3 "createFlag" room x' y' name'
createFlagWithName room (TargetPos pos) name' = runThisEffFn2 "createFlag" room pos name'

createFlagWithColor :: Room -> TargetPosition -> String -> Color -> Effect ReturnCode
createFlagWithColor room (TargetPt x' y') name' color = runThisEffFn4 "createFlag" room x' y' name' color
createFlagWithColor room (TargetPos pos) name' color = runThisEffFn3 "createFlag" room pos name' color

createFlagWithColors :: Room -> TargetPosition -> String -> Color -> Color -> Effect ReturnCode
createFlagWithColors room (TargetPt x' y') name' color color2 =
  runThisEffFn5 "createFlag" room x' y' name' color color2
createFlagWithColors room (TargetPos pos) name' color color2 =
  runThisEffFn4 "createFlag" room pos name' color color2

find :: forall a. Room -> FindType a -> Array a
find = runThisFn1 "find"

find' :: forall a. Room -> FindType a -> FilterFn a -> Array a
find' room findType filter = runThisFn2 "find" room findType { filter }

data RoomIdentifier = RoomName String | RoomObj Room

foreign import findExitToImpl :: forall a.
  Room ->
  a ->
  (ReturnCode -> Either ReturnCode (FindType RoomPosition)) ->
  (FindType RoomPosition -> Either ReturnCode (FindType RoomPosition)) ->
  Either ReturnCode (FindType RoomPosition)

findExitTo :: Room -> RoomIdentifier -> Either ReturnCode (FindType RoomPosition)
findExitTo room (RoomName otherRoomName) = findExitToImpl room otherRoomName Left Right
findExitTo room (RoomObj otherRoom) = findExitToImpl room otherRoom Left Right

findPath :: Room -> RoomPosition -> RoomPosition -> Path
findPath = runThisFn2 "findPath"

findPath' :: forall o. Room -> RoomPosition -> RoomPosition -> PathOptions o -> Path
findPath' room pos1 pos2 opts = runThisFn3 "findPath" room pos1 pos2 (selectMaybes opts)

getPositionAt :: Room -> Int -> Int -> RoomPosition
getPositionAt = runThisFn2 "getPositionAt"

-- lookAt omitted - use lookForAt
-- lookAtArea omitted - use lookForAtArea

lookForAt :: forall a. Room -> LookType a -> TargetPosition -> Array a
lookForAt room lookType (TargetPt x' y') = runThisFn3 "lookForAt" room lookType x' y'
lookForAt room lookType (TargetPos pos) = runThisFn2 "lookForAt" room lookType pos

-- TODO: implement this
-- lookForAtArea :: forall a. Room -> LookType a -> Int -> Int -> Int -> Int -> Boolean -> Array a
-- lookForAtArea r t top left bot right asArray = runThisFn6 "lookForAt" r t top left bot right asArray
