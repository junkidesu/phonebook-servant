{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api where

import Api.Persons (PersonsAPI, personsServer)
import Database.PostgreSQL.Simple
import Servant
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)

type API = PersonsAPI

type DocsAPI = API :<|> SwaggerSchemaUI "swagger.json" "swagger-ui"

server :: Connection -> Server DocsAPI
server conn = personsServer conn :<|> swaggerSchemaUIServer (toOpenApi (Proxy :: Proxy API))
