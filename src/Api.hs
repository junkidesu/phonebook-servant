{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api where

import Api.Auth (AuthAPI, authServer)
import Api.Persons (PersonsAPI, personsServer)
import Api.Users (UsersAPI, usersServer)
import Control.Lens
import Data.Pool (Pool)
import Data.Swagger
import Database.PostgreSQL.Simple
import Servant
import Servant.Auth.Server (JWTSettings)
import Servant.Auth.Swagger ()
import Servant.Swagger (HasSwagger (toSwagger), subOperations)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)

type API = AuthAPI :<|> UsersAPI :<|> PersonsAPI

type DocsAPI = API :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"

usersOpts :: Traversal' Swagger Operation
usersOpts = subOperations (Proxy :: Proxy UsersAPI) (Proxy :: Proxy API)

personsOpts :: Traversal' Swagger Operation
personsOpts = subOperations (Proxy :: Proxy PersonsAPI) (Proxy :: Proxy API)

authOpts :: Traversal' Swagger Operation
authOpts = subOperations (Proxy :: Proxy AuthAPI) (Proxy :: Proxy API)

swaggerDoc :: Swagger
swaggerDoc =
  toSwagger (Proxy :: Proxy API)
    & info . title .~ "Phonebook API"
    & info . version .~ "1.0"
    & info . description ?~ "Simple REST API written in Haskell with Servant"
    & info . license ?~ "BSD"
    & applyTagsFor authOpts ["auth" & description ?~ "Operations on auth"]
    & applyTagsFor usersOpts ["users" & description ?~ "Operations on users"]
    & applyTagsFor personsOpts ["persons" & description ?~ "Operations on persons"]

server :: Pool Connection -> JWTSettings -> Server DocsAPI
server conns jwts =
  ( authServer conns jwts
      :<|> usersServer conns
      :<|> personsServer conns
  )
    :<|> swaggerSchemaUIServer swaggerDoc
