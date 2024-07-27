{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Phonebook.Web.OpenApi (PhonebookOpenApi, server) where

import Control.Lens
import Data.Data (Proxy (Proxy))
import Data.OpenApi hiding (server)
import qualified Phonebook.Persons.Web as Persons
import qualified Phonebook.Users.Web as Users
import Phonebook.Web.API hiding (server)
import Phonebook.Web.AppM (AppM)
import Servant (HasServer (ServerT))
import Servant.Auth.OpenApi ()
import Servant.OpenApi
import Servant.Swagger.UI

type PhonebookOpenApi = SwaggerSchemaUI "swagger-ui" "swagger.json"

usersOpts :: Traversal' OpenApi Operation
usersOpts = subOperations (Proxy :: Proxy Users.API) (Proxy :: Proxy API)

personsOpts :: Traversal' OpenApi Operation
personsOpts = subOperations (Proxy :: Proxy Persons.API) (Proxy :: Proxy API)

openApiDoc :: OpenApi
openApiDoc =
  toOpenApi (Proxy :: Proxy API)
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
    & applyTagsFor personsOpts ["persons" & description ?~ "Operations on persons"]

server :: ServerT PhonebookOpenApi AppM
server = swaggerSchemaUIServerT openApiDoc
