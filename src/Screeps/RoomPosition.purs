-- | Corresponds to the Screeps API [RoomPosition](http://support.screeps.com/hc/en-us/articles/203079201-RoomPosition)
module Screeps.RoomPosition where

import Prelude

import Data.Either (Either, hush)
import Data.Maybe (Maybe(Nothing))
import Effect (Effect)
import Effect.Exception (Error, try)
import Screeps.FFI (runThisEffFn0, runThisEffFn1, runThisEffFn2, runThisEffFn3, runThisFn1, runThisFn2, runThisFn3, selectMaybes, toMaybe, unsafeField)
import Screeps.Room (PathOptions)
import Screeps.Types (Color, Direction, FilterFn, FindType, LookType, Path, ReturnCode, RoomPosition, StructureType, TargetPosition(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import mkRoomPosition :: Int -> Int -> String -> RoomPosition

type ClosestPathOptions a = PathOptions
  ( filter :: Maybe (a -> Boolean)
  , algorithm :: Maybe FindAlgorithm )

newtype FindAlgorithm = FindAlgorithm String

algorithmAstar :: FindAlgorithm
algorithmAstar = FindAlgorithm "astar"

algorithmDijkstra :: FindAlgorithm
algorithmDijkstra = FindAlgorithm "dijkstra"

closestPathOpts :: forall a. ClosestPathOptions a
closestPathOpts =
  { ignoreCreeps: Nothing
  , ignoreDestructibleStructures: Nothing
  , ignoreRoads: Nothing
  , ignore: Nothing
  , avoid: Nothing
  , maxOps: Nothing
  , heuristicWeight: Nothing
  , serialize: Nothing
  , maxRooms: Nothing
  , filter: Nothing
  , algorithm: Nothing
  }

unwrapContext :: forall a b. FindType a -> b
unwrapContext findType = unsafeCoerce findType

roomName :: RoomPosition -> String
roomName = unsafeField "roomName"

x :: RoomPosition -> Int
x = unsafeField "x"

y :: RoomPosition -> Int
y = unsafeField "y"

createConstructionSite :: RoomPosition -> StructureType -> Effect ReturnCode
createConstructionSite = runThisEffFn1 "createConstructionSite"

createFlag :: RoomPosition -> Effect ReturnCode
createFlag = runThisEffFn0 "createFlag"

createFlagWithName :: RoomPosition -> String -> Effect ReturnCode
createFlagWithName pos name = runThisEffFn1 "createFlag" pos name

createFlagWithColor :: RoomPosition -> String -> Color -> Effect ReturnCode
createFlagWithColor pos name color = runThisEffFn2 "createFlag" pos name color

createFlagWithColors :: RoomPosition -> String -> Color -> Color -> Effect ReturnCode
createFlagWithColors pos name color secondaryColor = runThisEffFn3 "createFlag" pos name color secondaryColor

findClosestByPath :: forall a. RoomPosition -> FindType a -> Effect (Maybe a)
findClosestByPath pos ctx = do
  result <- try (runThisEffFn1 "findClosestByPath" pos (unwrapContext ctx))
  pure (hush result >>= toMaybe)

findClosestByPath' :: forall a. RoomPosition -> FindType a -> ClosestPathOptions a -> Effect (Maybe a)
findClosestByPath' pos ctx opts = do
  result <- try (runThisEffFn2 "findClosestByPath" pos ctx' options)
  pure (hush result >>= toMaybe)

  where 
    ctx' = unwrapContext ctx
    options = selectMaybes opts

findClosestByRange :: forall a. RoomPosition -> FindType a -> Effect (Maybe a)
findClosestByRange pos ctx = do
  result <- try (runThisEffFn1 "findClosestByRange" pos (unwrapContext ctx))
  pure (hush result >>= toMaybe)

findClosestByRange' :: forall a. RoomPosition -> FindType a -> FilterFn a -> Effect (Maybe a)
findClosestByRange' pos ctx filter = do
  result <- try (runThisEffFn2 "findClosestByRange" pos (unwrapContext ctx) { filter })
  pure (hush result >>= toMaybe)

findInRange :: forall a. RoomPosition -> FindType a -> Int -> Effect (Either Error (Array a))
findInRange pos ctx range = try (runThisEffFn2 "findInRange" pos (unwrapContext ctx) range)

findInRange' :: forall a. RoomPosition -> FindType a -> Int -> FilterFn a -> Effect (Either Error (Array a))
findInRange' pos ctx range filter = try (runThisEffFn3 "findInRange" pos (unwrapContext ctx) range { filter })

findPathTo :: RoomPosition -> TargetPosition -> Effect (Either Error Path)
findPathTo pos (TargetPt x' y') = try (runThisEffFn2 "findPathTo" pos x' y')
findPathTo pos (TargetPos destPos) = try (runThisEffFn1 "findPathTo" pos destPos)

findPathTo' :: RoomPosition -> TargetPosition -> PathOptions () -> Effect (Either Error Path)
findPathTo' pos (TargetPt x' y') opts = try (runThisEffFn3 "findPathTo" pos x' y' (selectMaybes opts))
findPathTo' pos (TargetPos destPos) opts = try (runThisEffFn2 "findPathTo" pos destPos (selectMaybes opts))

getDirectionTo :: RoomPosition -> TargetPosition -> Direction
getDirectionTo pos (TargetPt x' y') = runThisFn2 "getDirectionTo" pos x' y'
getDirectionTo pos (TargetPos otherPos) = runThisFn1 "getDirectionTo" pos otherPos

-- | May return Infinity
getRangeTo :: RoomPosition -> TargetPosition -> Int
getRangeTo pos (TargetPt x' y') = runThisFn2 "getRangeTo" pos x' y'
getRangeTo pos (TargetPos destPos) = runThisFn1 "getRangeTo" pos destPos

inRangeTo :: RoomPosition -> TargetPosition -> Int -> Boolean
inRangeTo pos (TargetPt x' y') range = runThisFn3 "inRangeTo" pos x' y' range
inRangeTo pos (TargetPos destPos) range = runThisFn2 "inRangeTo" pos destPos range

isEqualTo :: RoomPosition -> TargetPosition -> Boolean
isEqualTo pos (TargetPt x' y') = runThisFn2 "isEqualTo" pos x' y'
isEqualTo pos (TargetPos otherPos) = runThisFn1 "isEqualTo" pos otherPos

isNearTo :: RoomPosition -> TargetPosition -> Boolean
isNearTo pos (TargetPt x' y') = runThisFn2 "isNearTo" pos x' y'
isNearTo pos (TargetPos otherPos) = runThisFn1 "isNearTo" pos otherPos

-- look function omitted - use lookFor

lookFor :: forall a. RoomPosition -> LookType a -> Effect (Either Error (Array a))
lookFor pos lookType = try (runThisEffFn1 "lookFor" pos lookType)
