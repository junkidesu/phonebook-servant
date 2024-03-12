{-# LANGUAGE DeriveGeneric #-}

module Types.AuthUser (AuthUser (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics

data AuthUser = AuthUser
  { userId :: !Text
  , token :: !Text
  }
  deriving (Generic)

instance FromJSON AuthUser
instance ToJSON AuthUser
instance ToSchema AuthUser
