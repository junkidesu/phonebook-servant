{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module DTO where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Database.SQLite.Simple

data NewPersonDTO = NewPersonDTO {newName :: String, newNumber :: Maybe String}

instance FromJSON NewPersonDTO where
  parseJSON :: Value -> Parser NewPersonDTO
  parseJSON = withObject "NewPersonDTO" $ \v ->
    NewPersonDTO
      <$> v .: "name"
      <*> v .: "number"

instance ToRow NewPersonDTO where
  toRow :: NewPersonDTO -> [SQLData]
  toRow np = toRow (newName np, newNumber np)