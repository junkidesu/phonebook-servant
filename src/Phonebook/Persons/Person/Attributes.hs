{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Phonebook.Persons.Person.Attributes where

import Data.Text (Text)

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity (Identity)
import Data.Swagger (ToSchema)
import GHC.Generics (Generic)
import Phonebook.Attribute

data Attributes f = Attributes
  { name :: Attribute f Text
  , number :: Attribute f Text
  }
  deriving (Generic)

type New = Attributes Identity
type Edit = Attributes Maybe

deriving instance Show New
deriving instance FromJSON New
deriving instance ToJSON New
deriving instance ToSchema New
