{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Types.EditPerson where

import Data.Aeson
import Data.Swagger
import Data.Text
import Database.PostgreSQL.Simple
import GHC.Generics (Generic)

data EditPerson = EditPerson
  { name :: !Text
  , number :: !Text
  }
  deriving (Generic)

instance FromJSON EditPerson
instance ToJSON EditPerson
instance ToRow EditPerson
instance ToSchema EditPerson
