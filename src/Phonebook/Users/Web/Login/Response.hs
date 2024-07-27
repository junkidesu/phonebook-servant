{-# LANGUAGE DeriveGeneric #-}

module Phonebook.Users.Web.Login.Response (Response (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

data Response = Response
  { token :: !Text
  , id :: !Int32
  , username :: !Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON Response
instance FromJSON Response
instance ToSchema Response
