{-# LANGUAGE DeriveGeneric #-}

module Types.LoginResponse (LoginResponse (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

data LoginResponse = LoginResponse
  { token :: !Text
  , id :: !Int
  , username :: !Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON LoginResponse
instance FromJSON LoginResponse
instance ToSchema LoginResponse
