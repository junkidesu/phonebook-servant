{-# LANGUAGE DeriveGeneric #-}

module DB.Model (Person) where

import Data.Aeson
import Database.SQLite.Simple
import GHC.Generics (Generic)

data Person = Person
  { id :: Int,
    name :: String,
    number :: Maybe String
  }
  deriving (Show, Eq, Generic)

instance ToJSON Person

instance FromRow Person