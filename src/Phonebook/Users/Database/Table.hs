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
  users,
  anwar,
  kuon,
  emily,
) where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres (Postgres)

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

anwar :: UserT (QExpr Postgres s)
kuon :: UserT (QExpr Postgres s)
emily :: UserT (QExpr Postgres s)
users :: [UserT (QExpr Postgres s)]
users@[anwar, kuon, emily] =
  [ User default_ (val_ "anwar") (val_ "$2b$10$6Wzy9.PcHfNmT8g3gvBJ5u5gwlmJLZZwuVe0UGZdHZItI2loqWqEW")
  , User default_ (val_ "kuon") (val_ "$2b$10$6Wzy9.PcHfNmT8g3gvBJ5u5gwlmJLZZwuVe0UGZdHZItI2loqWqEW")
  , User default_ (val_ "emily") (val_ "$2b$10$6Wzy9.PcHfNmT8g3gvBJ5u5gwlmJLZZwuVe0UGZdHZItI2loqWqEW")
  ]