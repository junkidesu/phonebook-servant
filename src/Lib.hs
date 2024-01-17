{-# LANGUAGE DataKinds #-}

module Lib
  ( startApp,
    app,
  )
where

import DB
import Database.SQLite.Simple
import Network.Wai
import Network.Wai.Handler.Warp
import API
import Servant

startApp :: IO ()
startApp = do
  db <- openDB
  run 8080 $ app db

app :: Connection -> Application
app = serve api . personsServer

api :: Proxy PersonAPI
api = Proxy
