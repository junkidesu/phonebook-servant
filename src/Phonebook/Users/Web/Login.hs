{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Users.Web.Login (Endpoint, handler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Password.Bcrypt
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection)
import Phonebook.Users.Database (toUserType)
import qualified Phonebook.Users.Database as Database
import qualified Phonebook.Users.User as User
import qualified Phonebook.Users.Web.Login.Credentials as Credentials
import qualified Phonebook.Users.Web.Login.Response as Response
import qualified Phonebook.Users.Web.Login.User as Login
import Servant
import Servant.Auth.Server (JWTSettings, makeJWT)

type Endpoint =
  "login"
    :> Summary "Login to the application"
    :> ReqBody '[JSON] Credentials.Credentials
    :> Post '[JSON] Response.Response

handler :: Pool Connection -> JWTSettings -> Credentials.Credentials -> Handler Response.Response
handler conns jwts credentials = login
 where
  login :: Handler Response.Response
  login = do
    mbUser <-
      liftIO $
        Database.userByUsername
          conns
          (Credentials.username credentials)

    case toUserType <$> mbUser of
      Nothing ->
        throwError err401{errBody = "Incorrect username or password"}
      Just user -> do
        case checkCredentials user of
          PasswordCheckFail ->
            throwError err401{errBody = "Incorrect username or password"}
          PasswordCheckSuccess -> do
            let userForToken = Login.User (User.id user) (User.username user)

            token <- liftIO $ generateToken userForToken

            return $
              Response.Response token (User.id user) (User.username user)

  checkCredentials :: User.User -> PasswordCheck
  checkCredentials user =
    checkPassword
      (mkPassword . Credentials.password $ credentials)
      (PasswordHash . User.password $ user)

  generateToken :: Login.User -> IO T.Text
  generateToken lu = do
    eitherToken <- makeJWT lu jwts Nothing

    case eitherToken of
      Left _ -> error "Could not generate token"
      Right tokenBS -> return $ T.pack . BLU.toString $ tokenBS
