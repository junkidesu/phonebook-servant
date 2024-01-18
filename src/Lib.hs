{-# LANGUAGE DataKinds #-}

module Lib
  ( startApp,
    app,
  )
where

import API
import DB.Operations
import Database.SQLite.Simple
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

startApp :: IO ()
startApp = do
  db <- openDB
  run 8080 $ app db

app :: Connection -> Application
app = serve api . personsServer

api :: Proxy PersonAPI
api = Proxy
