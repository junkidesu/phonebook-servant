{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Phonebook.Web.Swagger (PhonebookSwagger, server) where

import Control.Lens
import Data.Data (Proxy (Proxy))
import Data.Swagger
import qualified Phonebook.Users.Web as Users
import Phonebook.Web.API hiding (server)
import Servant (Server)
import Servant.Swagger
import Servant.Swagger.UI

type PhonebookSwagger = SwaggerSchemaUI "swagger-ui" "swagger.json"

usersOpts :: Traversal' Swagger Operation
usersOpts = subOperations (Proxy :: Proxy Users.API) (Proxy :: Proxy API)

swaggerDoc :: Swagger
swaggerDoc =
  toSwagger (Proxy :: Proxy API)
    & info
      . title
      .~ "Phonebook API"
    & info
      . version
      .~ "1.0"
    & info
      . description
      ?~ "Simple REST API written in Haskell with Servant"
    & info
      . license
      ?~ "BSD"
    & applyTagsFor usersOpts ["users" & description ?~ "Operations on users"]

server :: Server PhonebookSwagger
server = swaggerSchemaUIServer swaggerDoc
