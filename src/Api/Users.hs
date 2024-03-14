{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Users (UsersAPI, usersServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Db.Operations.Users
import Servant
import Servant.Auth
import Servant.Auth.Server
import qualified Types.AuthUser as AU
import qualified Types.NewUser as NU
import Types.User

type JWTAuth = Auth '[JWT] AU.AuthUser

type GetAllUsers = Summary "Get all users" :> Get '[JSON] [User]

type CreateUser =
  Summary "Create a new user"
    :> ReqBody'
        '[Required, Description "The username and password of the person"]
        '[JSON]
        NU.NewUser
    :> PostCreated '[JSON] User

type DeleteUser =
  JWTAuth
    :> Summary "Delete user with the given ID"
    :> Capture "id" Int
    :> Verb 'DELETE 204 '[JSON] NoContent

type UsersAPI = "users" :> (GetAllUsers :<|> CreateUser :<|> DeleteUser)

usersServer :: Pool Connection -> Server UsersAPI
usersServer conns = getAllUsers :<|> createUser :<|> removeUser
 where
  getAllUsers :: Handler [User]
  getAllUsers = liftIO $ allUsers conns

  createUser :: NU.NewUser -> Handler User
  createUser nu =
    if T.null (NU.password nu)
      then throwError err400{errBody = "Empty password!"}
      else liftIO $ insertUser conns nu

  removeUser :: AuthResult AU.AuthUser -> Int -> Handler NoContent
  removeUser (Authenticated au) userId = do
    if AU.id au /= userId
      then throwError err401
      else do
        liftIO $ deleteUser conns userId
        return NoContent
  removeUser _ _ = throwError err401
