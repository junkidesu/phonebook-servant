{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Web (startApp) where

import Configuration.Dotenv
import Control.Monad.Trans.Reader
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

startApp :: IO ()
startApp = do
  onMissingFile (loadFile defaultConfig) (putStrLn "Invalid environment")

  connectionPool <- connectToDb

  myKey <- generateKey

  let
    jwtCfg = defaultJWTSettings myKey
    cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
    app =
      serveWithContext api cfg $
        hoistServerWithContext
          api
          (Proxy :: Proxy '[CookieSettings, JWTSettings])
          (`runReaderT` Environment connectionPool)
          (server jwtCfg)
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 3003 $ setLogger aplogger defaultSettings
    runSettings settings app
