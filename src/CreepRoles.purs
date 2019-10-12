module CreepRoles (Role(..)) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut as JSON
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

data Role 
  = HarvesterRole
  | BuilderRole
  | UpgraderRole

instance showRole :: Show Role where
  show HarvesterRole = "harvester"
  show BuilderRole = "builder"
  show UpgraderRole = "upgrader"

instance decodeRole :: DecodeJson Role where
  decodeJson json 
    | Just "harvester" <- JSON.toString json = Right HarvesterRole
    | Just "builder" <- JSON.toString json = Right BuilderRole
    | Just "upgrader" <- JSON.toString json = Right UpgraderRole
    | otherwise = Left $ "unable to parse json as role:\n" <> JSON.stringify json
    
instance encodeRole :: EncodeJson Role where
  encodeJson HarvesterRole = JSON.fromString $ show HarvesterRole
  encodeJson BuilderRole = JSON.fromString $ show BuilderRole 
  encodeJson UpgraderRole = JSON.fromString $ show UpgraderRole

