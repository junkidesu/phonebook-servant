{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Phonebook.Persons.Database.Table (
  PersonT (..),
  Person,
  PersonId,
  PrimaryKey (..),
) where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam
import Phonebook.Users.Database.Table (UserT)

data PersonT f = Person
  { _personId :: C f Int32
  , _personName :: C f Text
  , _personNumber :: C f Text
  , _personUser :: PrimaryKey UserT f
  }
  deriving (Generic, Beamable)

type Person = PersonT Identity
type PersonId = PrimaryKey PersonT Identity

deriving instance Show Person
deriving instance Show PersonId

instance Table PersonT where
  data PrimaryKey PersonT f = PersonId (C f Int32) deriving (Generic, Beamable)
  primaryKey = PersonId . _personId
