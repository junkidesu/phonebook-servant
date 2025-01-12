{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Persons.Web.Avatar.Update (Endpoint, handler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (toStrict)
import Data.Int (Int32)
import qualified Data.Text as T
import Phonebook.Persons.Database (personById, toPersonType, updateAvatar)
import qualified Phonebook.Persons.Person as Person
import qualified Phonebook.Users.Web.Login.User as Login
import Phonebook.Web.AppM (AppM)
import Phonebook.Web.JWTAuth (JWTAuth)
import Phonebook.Web.Minio (uploadFile)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated), ThrowAll (throwAll))
import Servant.Multipart

type Endpoint =
  Summary "Update avatar of a person"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the person"] "id" Int32
    :> "avatar"
    :> MultipartForm Mem (MultipartData Mem)
    :> Post '[JSON] Person.Person

handler :: AuthResult Login.User -> Int32 -> MultipartData Mem -> AppM Person.Person
handler (Authenticated _) personId multipartData = do
  mbPersonResult <- personById personId

  case mbPersonResult of
    Nothing -> throwError err404
    Just _ -> do
      case lookupFile "file" multipartData of
        Left _ -> throwError err400
        Right fileData -> do
          let
            avatarFileExtension = T.dropWhile (/= '.') $ fdFileName fileData
            avatarPayload = toStrict $ fdPayload fileData
            avatarFileName =
              "avatars/user_"
                <> T.pack (show personId)
                <> "_avatar"
                <> avatarFileExtension
            avatarFileCType = fdFileCType fileData

          liftIO $ do
            print avatarFileExtension
            print avatarFileName

          res <-
            uploadFile
              avatarPayload
              avatarFileName
              avatarFileCType

          case res of
            Left _ -> throwError err400
            Right _ -> do
              updatedPerson <- updateAvatar personId avatarFileName
              return $ toPersonType updatedPerson
handler _ _ _ = throwAll err401
