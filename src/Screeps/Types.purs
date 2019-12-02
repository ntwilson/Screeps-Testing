-- | Defines the main types used in the library and the relationships between them.
module Screeps.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Map.Internal (Map)

foreign import data GameGlobal :: Type

class RoomObject a
class RoomObject a <= Structure a
class Structure a <= OwnedStructure a 

-- Some screeps collections contain many different types of structures in one non-homogenous collection.
-- This represents a parent type for any kind of structure.  Each concrete structure module has a `to_`
-- function to convert from this parent type to the concrete type. 
foreign import data SomeStructure :: Type
instance roomObjSomeStructure :: RoomObject SomeStructure
instance structureSomeStructure :: Structure SomeStructure

foreign import data Market :: Type
foreign import data Room :: Type
foreign import data RoomPosition :: Type
foreign import data WorldMap :: Type

foreign import data Container :: Type
instance roomObjContainer :: RoomObject Container
instance structureContainer :: Structure Container

foreign import data Ruin :: Type
instance roomObjRuin :: RoomObject Ruin
instance structureRuin :: Structure Ruin


foreign import data Controller :: Type
instance roomObjController :: RoomObject Controller
instance structureController :: Structure Controller
instance ownedStructController :: OwnedStructure Controller

foreign import data Extension :: Type
instance roomObjExtension :: RoomObject Extension
instance structureExtension :: Structure Extension
instance ownedStructExtension :: OwnedStructure Extension

foreign import data Extractor :: Type
instance roomObjExtractor :: RoomObject Extractor
instance structureExtractor :: Structure Extractor
instance ownedStructExtractor :: OwnedStructure Extractor

foreign import data KeeperLair :: Type
instance roomObjKeeperLair :: RoomObject KeeperLair
instance structureKeeperLair :: Structure KeeperLair
instance ownedStructKeeperLair :: OwnedStructure KeeperLair

foreign import data Lab :: Type
instance roomObjLab :: RoomObject Lab
instance structureLab :: Structure Lab
instance ownedStructLab :: OwnedStructure Lab

foreign import data Link :: Type
instance roomObjLink :: RoomObject Link
instance structureLink :: Structure Link
instance ownedStructLink :: OwnedStructure Link

foreign import data Nuker :: Type
instance roomObjNuker :: RoomObject Nuker
instance structureNuker :: Structure Nuker
instance ownedStructNuker :: OwnedStructure Nuker

foreign import data Observer :: Type
instance roomObjObserver :: RoomObject Observer
instance structureObserver :: Structure Observer
instance ownedStructObserver :: OwnedStructure Observer

foreign import data Portal :: Type
instance roomObjPortal :: RoomObject Portal
instance structurePortal :: Structure Portal
instance ownedStructPortal :: OwnedStructure Portal

foreign import data PowerBank :: Type
instance roomObjPowerBank :: RoomObject PowerBank
instance structurePowerBank :: Structure PowerBank
instance ownedStructPowerBank :: OwnedStructure PowerBank

foreign import data PowerSpawn :: Type
instance roomObjPowerSpawn :: RoomObject PowerSpawn
instance structurePowerSpawn :: Structure PowerSpawn
instance ownedStructPowerSpawn :: OwnedStructure PowerSpawn

foreign import data Rampart :: Type
instance roomObjRampart :: RoomObject Rampart
instance structureRampart :: Structure Rampart
instance ownedStructRampart :: OwnedStructure Rampart

foreign import data Road :: Type
instance roomObjRoad :: RoomObject Road
instance structureRoad :: Structure Road
instance ownedStructRoad :: OwnedStructure Road

foreign import data Spawn :: Type
instance roomObjSpawn :: RoomObject Spawn
instance structureSpawn :: Structure Spawn
instance ownedStructSpawn :: OwnedStructure Spawn

foreign import data Storage :: Type
instance roomObjStorage :: RoomObject Storage
instance structureStorage :: Structure Storage
instance ownedStructStorage :: OwnedStructure Storage

foreign import data Terminal :: Type
instance roomObjTerminal :: RoomObject Terminal
instance structureTerminal :: Structure Terminal
instance ownedStructTerminal :: OwnedStructure Terminal

foreign import data Tower :: Type
instance roomObjTower :: RoomObject Tower
instance structureTower :: Structure Tower
instance ownedStructTower :: OwnedStructure Tower

foreign import data Wall :: Type
instance roomObjWall :: RoomObject Wall
instance structureWall :: Structure Wall
instance ownedStructWall :: OwnedStructure Wall


foreign import data Tombstone :: Type
instance roomObjTombstone :: RoomObject Tombstone

foreign import data ConstructionSite :: Type
instance roomObjConstructionSite :: RoomObject ConstructionSite

foreign import data Creep :: Type
instance roomObjCreep :: RoomObject Creep

foreign import data Flag :: Type
instance roomObjFlag :: RoomObject Flag

foreign import data Mineral :: Type
instance roomObjMineral :: RoomObject Mineral

foreign import data Nuke :: Type
instance roomObjNuke :: RoomObject Nuke

foreign import data Resource :: Type
instance roomObjResource :: RoomObject Resource

foreign import data Source :: Type
instance roomObjSource :: RoomObject Source





foreign import data Store :: Type

type Path = Array PathStep -- or String?

type PathStep =
  { x :: Int
  , y :: Int
  , dx :: Number
  , dy :: Number
  , direction :: Direction }

newtype ReturnCode = ReturnCode Int
derive instance genericReturnCode :: Generic ReturnCode _
instance eqReturnCode :: Eq ReturnCode where eq = genericEq
instance showReturnCode :: Show ReturnCode where
  show (ReturnCode n) = show n

newtype ResourceType = ResourceType String
derive instance genericResourceType :: Generic ResourceType _
instance eqResourceType :: Eq ResourceType where eq = genericEq
instance showResourceType :: Show ResourceType where
  show (ResourceType s) = s

newtype StructureType = StructureType String
derive instance genericStructureType :: Generic StructureType _
instance eqStructureType :: Eq StructureType where eq = genericEq
instance showStructureType :: Show StructureType where show = genericShow

newtype TerrainMask = TerrainMask Int
derive instance genericTerrainMask :: Generic TerrainMask _
instance eqTerrainMask :: Eq TerrainMask where eq = genericEq
instance showTerrainMask :: Show TerrainMask where show = genericShow

newtype Terrain = Terrain String
derive instance genericTerrain :: Generic Terrain _
instance eqTerrain :: Eq Terrain where eq = genericEq
instance showTerrain :: Show Terrain
  where show (Terrain s) = s

newtype Mode = Mode String
derive instance genericMode :: Generic Mode _
instance eqMode :: Eq Mode where eq = genericEq
instance showMode :: Show Mode where show = genericShow

newtype Id a = Id String
derive instance genericId :: Generic (Id a) _
instance eqId :: Eq (Id a) where eq = genericEq
instance showId :: Show (Id a) where show = genericShow

newtype Direction = Direction Int
derive instance genericDirection :: Generic Direction _
instance eqDirection :: Eq Direction where eq = genericEq
instance showDirection :: Show Direction where show = genericShow

newtype BodyPartType = BodyPartType String
derive instance genericBodyPartType :: Generic BodyPartType _
instance eqBodyPartType :: Eq BodyPartType where eq = genericEq
instance showBodyPartType :: Show BodyPartType where show = genericShow

newtype Color = Color Int
derive instance genericColor :: Generic Color _
instance eqColor :: Eq Color where eq = genericEq
instance showColor :: Show Color where show = genericShow

newtype LookType a = LookType String
newtype FindType a = FindType Int

type StructureInfo = Map String Int

-----------------
-- Helper types and functions
-----------------

type FilterFn a = a -> Boolean

data TargetPosition 
  = TargetPt Int Int
  | TargetPos RoomPosition
--  | TargetObj (forall a. RoomObject a => a)


