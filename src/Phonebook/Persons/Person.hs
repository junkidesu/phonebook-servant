{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Phonebook.Persons.Person (Person (..)) where

import Data.Aeson
import Data.Int (Int32)
import Data.Swagger (ToSchema)
import Data.Text
import GHC.Generics (Generic)
import Phonebook.Users.User (User)

data Person = Person
  { id :: !Int32
  , name :: !Text
  , number :: !Text
  , author :: !User
  }
  deriving (Show, Generic)

instance ToJSON Person
instance ToSchema Person
