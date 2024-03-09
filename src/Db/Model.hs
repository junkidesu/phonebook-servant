{-# LANGUAGE DeriveGeneric #-}

module Db.Model (Person (Person)) where

import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Text
import Database.PostgreSQL.Simple
import GHC.Generics (Generic)

data Person = Person
  { id :: !Int
  , name :: !Text
  , number :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance ToJSON Person
instance FromRow Person
instance ToSchema Person
