{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api where

import Api.Persons (PersonsAPI, personsServer)
import Api.Users (UsersAPI, usersServer)
import Control.Lens
import Data.OpenApi hiding (Server)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple
import Servant
import Servant.OpenApi (HasOpenApi (toOpenApi), subOperations)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)

type API = UsersAPI :<|> PersonsAPI

type DocsAPI = API :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"

usersOpts :: Traversal' OpenApi Operation
usersOpts = subOperations (Proxy :: Proxy UsersAPI) (Proxy :: Proxy API)

personsOpts :: Traversal' OpenApi Operation
personsOpts = subOperations (Proxy :: Proxy PersonsAPI) (Proxy :: Proxy API)

openapiDoc :: OpenApi
openapiDoc =
  toOpenApi (Proxy :: Proxy API)
    & info . title .~ "Phonebook API"
    & info . version .~ "1.0"
    & info . description ?~ "Simple REST API written in Haskell with Servant"
    & info . license ?~ "BSD"
    & servers .~ ["http://localhost:8080", "https://phonebook-servant-main.onrender.com"]
    & applyTagsFor usersOpts ["users" & description ?~ "Operations on users"]
    & applyTagsFor personsOpts ["persons" & description ?~ "Operations on persons"]

server :: Pool Connection -> Server DocsAPI
server conns =
  ( usersServer conns
      :<|> personsServer conns
  )
    :<|> swaggerSchemaUIServer openapiDoc
