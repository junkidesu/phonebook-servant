{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Users.Web.Register (Endpoint, handler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Password.Bcrypt
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import qualified Phonebook.Users.Database as Database
import Phonebook.Users.User
import qualified Phonebook.Users.User.Attributes as Attributes
import Servant

type Endpoint =
  Summary "Create a new user"
    :> ReqBody'
        '[Required, Description "The username and password of the person"]
        '[JSON]
        Attributes.NewUser
    :> PostCreated '[JSON] User

handler :: Pool Connection -> Attributes.NewUser -> Handler User
handler conns nu = do
  hashedPw <- liftIO . hashPassword . mkPassword $ Attributes.password nu

  Database.toUserType
    <$> liftIO
      ( Database.createUser conns $
          Attributes.Attributes
            (Attributes.username nu)
            (unPasswordHash hashedPw)
      )
