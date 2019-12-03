-- | Corresponds to the Screeps API [StructureLab](http://support.screeps.com/hc/en-us/articles/208436195-StructureLab)
module Screeps.Lab where

import Effect (Effect)
import Data.Maybe (Maybe)

import Screeps.Constants (structure_lab)
import Screeps.FFI (runThisEffFn1, runThisEffFn2, unsafeField)
import Screeps.Structure (unsafeCast)
import Screeps.Types (Creep, Lab, ReturnCode, class Structure)

cooldown :: Lab -> Int
cooldown = unsafeField "cooldown"

mineralType :: Lab -> String
mineralType = unsafeField "mineralType"

boostCreep :: Lab -> Creep -> Effect ReturnCode
boostCreep = runThisEffFn1 "boostCreep"

boostCreep' :: Lab -> Creep -> Int -> Effect ReturnCode
boostCreep' lab creep bodyPartsCount = runThisEffFn2 "boostCreep" lab creep bodyPartsCount

runReaction :: Lab -> Lab -> Lab -> Effect ReturnCode
runReaction lab lab1 lab2 = runThisEffFn2 "runReaction" lab lab1 lab2

toLab :: forall a. Structure a => a -> Maybe Lab
toLab = unsafeCast structure_lab
