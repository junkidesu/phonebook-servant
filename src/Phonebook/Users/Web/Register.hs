{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Users.Web.Register (Endpoint, handler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Password.Bcrypt
import qualified Phonebook.Users.Database as Database
import Phonebook.Users.User
import qualified Phonebook.Users.User.Attributes as Attributes
import Phonebook.Web.AppM (AppM)
import Servant

type Endpoint =
  Summary "Create a new user"
    :> ReqBody'
        '[Required, Description "The username and password of the person"]
        '[JSON]
        Attributes.New
    :> PostCreated '[JSON] User

handler :: Attributes.New -> AppM User
handler nu = do
  hashedPw <- liftIO . hashPassword . mkPassword $ Attributes.password nu

  Database.toUserType
    <$> Database.createUser
      ( Attributes.Attributes
          (Attributes.username nu)
          (unPasswordHash hashedPw)
      )
