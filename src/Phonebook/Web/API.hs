module Phonebook.Web.API (API, server) where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import qualified Phonebook.Users.Web as Users
import Servant
import Servant.Auth.Server (JWTSettings)

type API = Users.API

server :: Pool Connection -> JWTSettings -> Server API
server conns jwts = Users.server conns jwts
