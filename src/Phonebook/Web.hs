{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Web (startApp) where

import Configuration.Dotenv
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Phonebook.Database (connectToDb)
import qualified Phonebook.Web.API as API
import qualified Phonebook.Web.Swagger as Swagger
import Servant
import Servant.Auth.Server (JWTSettings, defaultCookieSettings, defaultJWTSettings, generateKey)
import Servant.Auth.Swagger ()

type Phonebook = API.API :<|> Swagger.PhonebookSwagger

server :: Pool Connection -> JWTSettings -> Server Phonebook
server conns jwts = API.server conns jwts :<|> Swagger.server

api :: Proxy Phonebook
api = Proxy

startApp :: IO ()
startApp = do
  onMissingFile (loadFile defaultConfig) (putStrLn "Invalid environment")

  conns <- connectToDb

  myKey <- generateKey

  let
    jwtCfg = defaultJWTSettings myKey
    cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
    app = serveWithContext api cfg (server conns jwtCfg)
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8080 $ setLogger aplogger defaultSettings
    runSettings settings app
