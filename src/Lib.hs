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
import Servant

startApp :: IO ()
startApp = do
  conn <- connectToDb
  run 8080 $ app conn

app :: Connection -> Application
app = serve api . server

api :: Proxy DocsAPI
api = Proxy
