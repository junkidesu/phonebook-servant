{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Auth (AuthAPI, authServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Password.Bcrypt
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection)
import Db.Operations.Users (userByUsername)
import Servant
import Servant.Auth.Server (JWTSettings, makeJWT)
import Types.AuthUser (AuthUser (AuthUser))
import qualified Types.Credentials as C
import Types.LoginResponse (LoginResponse (LoginResponse))
import qualified Types.User as U

type AuthAPI = "auth" :> ReqBody '[JSON] C.Credentials :> Post '[JSON] LoginResponse

authServer :: Pool Connection -> JWTSettings -> Server AuthAPI
authServer conns jwts = login
 where
  login :: C.Credentials -> Handler LoginResponse
  login credentials = do
    mbUser <-
      liftIO $
        userByUsername
          conns
          (C.username credentials)

    case mbUser of
      Nothing ->
        throwError err401{errBody = "Incorrect username or password"}
      Just user -> do
        case checkCredentials credentials user of
          PasswordCheckFail ->
            throwError err401{errBody = "Incorrect username or password"}
          PasswordCheckSuccess -> do
            let userForToken = AuthUser (U.id user) (U.username user)

            token <- liftIO $ generateToken userForToken

            return $
              LoginResponse token (U.id user) (U.username user)

  checkCredentials :: C.Credentials -> U.User -> PasswordCheck
  checkCredentials credentials user =
    checkPassword
      (mkPassword . C.password $ credentials)
      (PasswordHash . U.passwordHash $ user)

  generateToken :: AuthUser -> IO Text
  generateToken au = do
    eitherToken <- makeJWT au jwts Nothing

    case eitherToken of
      Left _ -> error "Could not generate token"
      Right tokenBS -> return $ T.pack . BLU.toString $ tokenBS
