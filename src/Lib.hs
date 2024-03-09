{-# LANGUAGE DataKinds #-}

module Lib (
  startApp,
  app,
)
where

import API
import DB.Operations
import Database.PostgreSQL.Simple
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

startApp :: IO ()
startApp = do
  conn <- connectToDb
  run 8080 $ app conn

app :: Connection -> Application
app = serve api . personsServer

api :: Proxy PersonAPI
api = Proxy
