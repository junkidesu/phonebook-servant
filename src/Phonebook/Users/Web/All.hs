{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Users.Web.All (Endpoint, handler) where

import qualified Phonebook.Users.Database as Database
import Phonebook.Users.User
import Phonebook.Web.AppM (AppM)
import Servant

type Endpoint = Summary "Get all users" :> Get '[JSON] [User]

handler :: AppM [User]
handler = map Database.toUserType <$> Database.allUsers
