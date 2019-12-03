-- | Corresponds to the Screeps API [Structure](http://support.screeps.com/hc/en-us/articles/203079221-Structure)
module Screeps.Structure where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Screeps.FFI (runThisEffFn0, runThisEffFn1, unsafeField)
import Screeps.Types (Id, ReturnCode, class Structure, StructureType)
import Unsafe.Coerce (unsafeCoerce)

id :: forall a. Structure a => a -> Id a
id = unsafeField "id"

structureType :: forall a. Structure a => a -> StructureType
structureType = unsafeField "structureType"

destroy :: forall a. Structure a => a -> Effect ReturnCode
destroy = runThisEffFn0 "destroy"

isActive :: forall a. Structure a => a -> Effect Boolean
isActive = runThisEffFn0 "isActive"

notifyWhenAttacked :: forall a. Structure a => a -> Boolean -> Effect ReturnCode
notifyWhenAttacked = runThisEffFn1 "notifyWhenAttacked"

unsafeCast :: forall a b. Structure a => StructureType -> a -> Maybe b
unsafeCast t struc
  | structureType struc == t = Just $ unsafeCoerce struc
  | otherwise = Nothing

