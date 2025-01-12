{-# LANGUAGE OverloadedStrings #-}

module Phonebook.Web.Minio where

import Conduit (MonadIO (liftIO), yield)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Minio
import Phonebook.Web.AppM (AppM)
import Phonebook.Web.Environment (Environment (minioConnection))

startMinio :: IO MinioConn
startMinio = do
  manager <- newManager defaultManagerSettings

  Just creds <- findFirst [fromMinioEnv, fromAWSEnv]

  mkMinioConn
    (setCreds creds "http://127.0.0.1:9000")
    manager

uploadFileExample :: IO ()
uploadFileExample = do
  loadFile defaultConfig

  conn <- startMinio

  image <- BS.readFile "image.jpg"

  res <-
    runMinioWith conn $
      putObject "phonebook-bucket" "avatars/image.jpg" (yield image) Nothing defaultPutObjectOptions

  case res of
    Right _ -> putStrLn "success!"
    Left e -> print e

uploadFile :: BS.ByteString -> T.Text -> T.Text -> AppM (Either MinioErr ())
uploadFile file filePath fileCType = do
  conn <- asks minioConnection

  liftIO $
    runMinioWith conn $
      putObject "phonebook-bucket" filePath (yield file) Nothing (defaultPutObjectOptions{pooContentType = Just fileCType})
