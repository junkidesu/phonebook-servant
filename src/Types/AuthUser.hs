{-# LANGUAGE DeriveGeneric #-}

module Types.AuthUser (AuthUser (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics
import Servant.Auth.JWT (FromJWT, ToJWT)

data AuthUser = AuthUser
  { id :: !Int
  , username :: !Text
  }
  deriving (Generic)

instance FromJSON AuthUser
instance ToJSON AuthUser
instance ToSchema AuthUser
instance ToJWT AuthUser
instance FromJWT AuthUser
