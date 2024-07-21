{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Phonebook.Users.User.Attributes (Attributes (..), New) where

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

type New = Attributes Identity

deriving instance FromJSON New
deriving instance ToJSON New
deriving instance ToSchema New
deriving instance Show New
