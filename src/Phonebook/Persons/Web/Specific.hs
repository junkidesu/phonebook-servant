{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Persons.Web.Specific (Endpoint, handler) where

import Data.Int (Int32)
import Phonebook.Persons.Database (personById, toPersonType)
import qualified Phonebook.Persons.Person as Person
import Phonebook.Web.AppM (AppM)
import Servant

type Endpoint =
  Summary "Get person by ID"
    :> Capture' '[Required, Description "ID of the person"] "id" Int32
    :> Get '[JSON] Person.Person

handler :: Int32 -> AppM Person.Person
handler personId = do
  mbPersonResult <- personById personId

  case mbPersonResult of
    Nothing -> throwError err404
    Just personResult -> pure $ toPersonType personResult
