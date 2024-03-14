{-# LANGUAGE DataKinds #-}

module Lib (
  startApp,
)
where

import Api
import Db
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant
import Servant.Auth.Server (defaultCookieSettings, defaultJWTSettings, generateKey)
import Util.ErrorFormatters (customFormatters)

startApp :: IO ()
startApp = do
  conns <- connectToDb

  myKey <- generateKey

  let
    jwtCfg = defaultJWTSettings myKey
    cfg = defaultCookieSettings :. jwtCfg :. customFormatters :. EmptyContext
    app = serveWithContext api cfg (server conns jwtCfg)
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8080 $ setLogger aplogger defaultSettings
    runSettings settings app

api :: Proxy DocsAPI
api = Proxy
