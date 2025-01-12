{-# LANGUAGE DeriveGeneric #-}

module Phonebook.Persons.Person (Person (..)) where

import Data.Aeson
import Data.Int (Int32)
import Data.OpenApi (ToSchema)
import Data.Text
import GHC.Generics (Generic)
import Phonebook.Users.User (User)

data Person = Person
  { id :: !Int32
  , name :: !Text
  , number :: !Text
  , avatar :: !(Maybe Text)
  , author :: !User
  }
  deriving (Show, Generic)

instance ToJSON Person
instance ToSchema Person
