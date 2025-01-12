module Phonebook.Web.Environment (Environment (..)) where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.Minio (MinioConn)

data Environment = Environment
  { connectionPool :: Pool Connection
  , minioConnection :: MinioConn
  }
