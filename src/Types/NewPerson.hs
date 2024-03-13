{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Types.NewPerson where

import Data.Aeson
import Data.Swagger
import Data.Text
import Database.PostgreSQL.Simple
import GHC.Generics (Generic)

data NewPerson = NewPerson
  { name :: !Text
  , number :: !(Maybe Text)
  }
  deriving (Generic)

instance FromJSON NewPerson
instance ToJSON NewPerson
instance ToRow NewPerson
instance ToSchema NewPerson
