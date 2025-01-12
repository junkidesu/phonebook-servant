{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Web (startApp) where

import Configuration.Dotenv
import Control.Monad.Trans.Reader
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Minio
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Phonebook.Database (connectToDb)
import qualified Phonebook.Web.API as API
import Phonebook.Web.AppM (AppM)
import Phonebook.Web.Environment (Environment (Environment))
import qualified Phonebook.Web.OpenApi as OpenApi
import Servant
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings, generateKey)

type Phonebook = API.API :<|> OpenApi.PhonebookOpenApi

server :: JWTSettings -> ServerT Phonebook AppM
server jwts = API.server jwts :<|> OpenApi.server

api :: Proxy Phonebook
api = Proxy

connectMinio :: IO MinioConn
connectMinio = do
  manager <- newManager defaultManagerSettings

  Just creds <- findFirst [fromMinioEnv, fromAWSEnv]

  mkMinioConn
    (setCreds creds "http://127.0.0.1:9000")
    manager

startApp :: IO ()
startApp = do
  onMissingFile (loadFile defaultConfig) (putStrLn "Invalid environment")

  connectionPool <- connectToDb

  minioConn <- connectMinio

  myKey <- generateKey

  let
    jwtCfg = defaultJWTSettings myKey
    cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
    app =
      serveWithContext api cfg $
        hoistServerWithContext
          api
          (Proxy :: Proxy '[CookieSettings, JWTSettings])
          (`runReaderT` Environment connectionPool minioConn)
          (server jwtCfg)
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 3003 $ setLogger aplogger defaultSettings
    runSettings settings app
