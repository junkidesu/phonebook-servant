module Phonebook.Web.Environment (Environment (..)) where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)

data Environment = Environment
  { connectionPool :: Pool Connection
  }
