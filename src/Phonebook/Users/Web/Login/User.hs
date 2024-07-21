{-# LANGUAGE DeriveGeneric #-}

module Phonebook.Users.Web.Login.User (User (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics
import Servant.Auth.JWT (FromJWT, ToJWT)

data User = User
  { id :: !Int32
  , username :: !Text
  }
  deriving (Generic)

instance FromJSON User
instance ToJSON User
instance ToSchema User
instance ToJWT User
instance FromJWT User
