{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Db.Operations.Users where

import Data.Password.Bcrypt
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Db.Queries.Users
import qualified Types.NewUser as NU
import Types.User (User)

insertUser :: Pool Connection -> NU.NewUser -> IO User
insertUser conns nu = do
  let pw = mkPassword (NU.password nu)
  hashedPw <- hashPassword pw
  [user] <- withResource conns $
    \conn -> query conn insertUserQ (NU.username nu, unPasswordHash hashedPw)
  return user

allUsers :: Pool Connection -> IO [User]
allUsers conns = withResource conns $
  \conn -> query_ conn allUsersQ

userByUsername :: Pool Connection -> Text -> IO (Maybe User)
userByUsername conns username = do
  withResource conns $
    \conn -> do
      res <- query conn userByUsernameQ (Only username) :: IO [User]
      case res of
        [] -> return Nothing
        (user : _) -> return $ Just user

deleteUser :: Pool Connection -> Int -> IO ()
deleteUser conns userId = do
  _ <- withResource conns $
    \conn -> execute conn deleteUserQ (Only userId)
  return ()
