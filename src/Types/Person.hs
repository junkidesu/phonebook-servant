{-# LANGUAGE DeriveGeneric #-}

module Types.Person (Person (Person)) where

import Data.Aeson
import Data.Swagger (ToSchema)
import Data.Text
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import GHC.Generics (Generic)
import Types.User

data Person = Person
  { id :: !Int
  , name :: !Text
  , number :: !Text
  , author :: !User
  }
  deriving (Show, Eq, Generic)

instance ToJSON Person
instance FromRow Person where
  fromRow =
    Person
      <$> field
      <*> field
      <*> field
      <*> ( User
              <$> field
              <*> field
              <*> field
          )

instance ToSchema Person
