{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module DTO where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Database.SQLite.Simple
import GHC.Generics (Generic)

data NewPersonDTO = NewPersonDTO {newName :: String, newNumber :: Maybe String} deriving (Generic)

instance FromJSON NewPersonDTO where
  parseJSON :: Value -> Parser NewPersonDTO
  parseJSON = withObject "NewPersonDTO" $ \v ->
    NewPersonDTO
      <$> v .: "name"
      <*> v .: "number"

instance ToRow NewPersonDTO

newtype UpdatePersonDTO = UpdatePersonDTO {changedNumber :: String} deriving (Generic)

instance FromJSON UpdatePersonDTO where
  parseJSON :: Value -> Parser UpdatePersonDTO
  parseJSON = withObject "UpdatePersonDTO" $ \v ->
    UpdatePersonDTO <$> v .: "number"

instance ToRow UpdatePersonDTO
