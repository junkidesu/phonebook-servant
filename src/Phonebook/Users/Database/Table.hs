{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Phonebook.Users.Database.Table (
  UserT (..),
  PrimaryKey (..),
  User,
  UserId,
) where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam

data UserT f = User
  { _userId :: !(C f Int32)
  , _userUsername :: !(C f Text)
  , _userPassword :: !(C f Text)
  }
  deriving (Generic, Beamable)

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Show UserId

instance Table UserT where
  data PrimaryKey UserT f = UserId (C f Int32) deriving (Generic, Beamable)
  primaryKey = UserId . _userId
