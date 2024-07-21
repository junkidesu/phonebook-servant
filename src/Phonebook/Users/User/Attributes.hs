{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Phonebook.Users.User.Attributes (Attributes (..), NewUser) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity (Identity)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Phonebook.Attribute (Attribute)

data Attributes f = Attributes
  { username :: Attribute f Text
  , password :: Attribute f Text
  }
  deriving (Generic)

type NewUser = Attributes Identity

deriving instance FromJSON NewUser
deriving instance ToJSON NewUser
deriving instance ToSchema NewUser
deriving instance Show NewUser
