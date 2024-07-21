{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Users.Web.All (Endpoint, handler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database.Beam.Postgres (Connection)
import qualified Phonebook.Users.Database as Database
import Phonebook.Users.User
import Servant

type Endpoint = Summary "Get all users" :> Get '[JSON] [User]

handler :: Pool Connection -> Handler [User]
handler conns =
  liftIO $
    map Database.toUserType <$> Database.allUsers conns
