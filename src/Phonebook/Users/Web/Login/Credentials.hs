{-# LANGUAGE DeriveGeneric #-}

module Phonebook.Users.Web.Login.Credentials (Credentials (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

data Credentials = Credentials
  { username :: !Text
  , password :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Credentials
instance ToJSON Credentials
instance ToSchema Credentials
