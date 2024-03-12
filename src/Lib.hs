{-# LANGUAGE DataKinds #-}

module Lib (
  startApp,
  app,
)
where

import Api
import Data.Pool (Pool)
import Database.PostgreSQL.Simple hiding ((:.))
import Db
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant
import Util.Error (customFormatters)

startApp :: IO ()
startApp = do
  conns <- connectToDb

  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8080 $ setLogger aplogger defaultSettings
    runSettings settings (app conns)

app :: Pool Connection -> Application
app = serveWithContext api (customFormatters :. EmptyContext) . server

api :: Proxy DocsAPI
api = Proxy
