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
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection)
import Db.Operations (userByUsername)
import Servant
import Servant.Auth.Server (JWTSettings, makeJWT)
import Types.AuthUser (AuthUser (AuthUser))
import qualified Types.Credentials as C
import Types.LoginResponse (LoginResponse (LoginResponse))
import qualified Types.User as U

type AuthAPI = "auth" :> ReqBody '[JSON] C.Credentials :> Post '[JSON] LoginResponse

authServer :: Pool Connection -> JWTSettings -> Server AuthAPI
authServer conns jwtCfg = login
 where
  login :: C.Credentials -> Handler LoginResponse
  login credentials = do
    mbUser <- liftIO $ userByUsername conns (C.username credentials)
    case mbUser of
      Nothing -> throwError err401{errBody = "Incorrect username or password"}
      Just user -> do
        case checkPassword (mkPassword . C.password $ credentials) (PasswordHash $ U.passwordHash user) of
          PasswordCheckFail -> throwError err401{errBody = "Incorrect username or password"}
          PasswordCheckSuccess -> do
            tokenRes <- liftIO $ makeJWT (AuthUser (U.id user) (U.username user)) jwtCfg Nothing

            case tokenRes of
              Left _ -> throwError err401
              Right tokenBS -> do
                let token = T.pack . BLU.toString $ tokenBS
                return $ LoginResponse token (U.id user) (U.username user)
