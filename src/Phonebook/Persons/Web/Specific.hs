{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Persons.Web.Specific (Endpoint, handler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Int (Int32)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Phonebook.Persons.Database (personById, toPersonType)
import qualified Phonebook.Persons.Person as Person
import Servant

type Endpoint =
  Summary "Get person by ID"
    :> Capture' '[Required, Description "ID of the person"] "id" Int32
    :> Get '[JSON] Person.Person

handler :: Pool Connection -> Int32 -> Handler Person.Person
handler conns personId = do
  mbPersonResult <- liftIO $ personById conns personId

  case mbPersonResult of
    Nothing -> throwError err404
    Just personResult -> pure $ toPersonType personResult
