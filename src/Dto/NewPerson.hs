{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Dto.NewPerson where

import Data.Aeson
import Data.OpenApi
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
