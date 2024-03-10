{-# LANGUAGE DataKinds #-}

module Lib (
  startApp,
  app,
)
where

import Api
import Database.PostgreSQL.Simple
import Db.Operations
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant

startApp :: IO ()
startApp = do
  conn <- connectToDb

  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8080 $ setLogger aplogger defaultSettings
    runSettings settings (app conn)

app :: Connection -> Application
app = serve api . server

api :: Proxy DocsAPI
api = Proxy
