module Phonebook.Users.Database (
  allUsers,
  userByUsername,
  userById,
  createUser,
  deleteUser,
  toUserType,
) where

import Data.Int (Int32)
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList))
import Database.Beam.Postgres (Connection, Postgres, runBeamPostgres)
import Phonebook.Database (PhonebookDb (phonebookUsers), db)
import Phonebook.Users.Database.Table
import qualified Phonebook.Users.User as User
import qualified Phonebook.Users.User.Attributes as Attributes

selectAllUsers :: Q Postgres PhonebookDb s (UserT (QExpr Postgres s))
selectAllUsers = all_ $ phonebookUsers db

selectUserByUsername :: Text -> Q Postgres PhonebookDb s (UserT (QExpr Postgres s))
selectUserByUsername username =
  filter_
    (\user -> _userUsername user ==. val_ username)
    selectAllUsers

selectUserById :: Int32 -> Q Postgres PhonebookDb s (UserT (QExpr Postgres s))
selectUserById userId =
  filter_
    (\user -> _userId user ==. val_ userId)
    selectAllUsers

allUsers :: Pool Connection -> IO [User]
allUsers conns = withResource conns $ \conn -> do
  runBeamPostgres conn $ runSelectReturningList $ select selectAllUsers

userByUsername :: Pool Connection -> Text -> IO (Maybe User)
userByUsername conns username = withResource conns $ \conn -> do
  runBeamPostgres conn $
    runSelectReturningFirst $
      select $
        selectUserByUsername username

userById :: Pool Connection -> Int32 -> IO (Maybe User)
userById conns userId =
  withResource conns $ \conn ->
    runBeamPostgres conn
      . runSelectReturningFirst
      . select
      . selectUserById
      $ userId

createUser :: Pool Connection -> Attributes.New -> IO User
createUser conns nu = withResource conns $ \conn -> do
  [insertedUser] <-
    runBeamPostgres conn $
      runInsertReturningList $
        insert (phonebookUsers db) $
          insertExpressions
            [ User
                default_
                (val_ . Attributes.username $ nu)
                (val_ . Attributes.password $ nu)
            ]
  return insertedUser

deleteUser :: Pool Connection -> Int32 -> IO ()
deleteUser conns userId = withResource conns $ \conn -> do
  runBeamPostgres conn $
    runDelete $
      delete (phonebookUsers db) (\user -> pk user ==. val_ (UserId userId))

toUserType :: User -> User.User
toUserType = User.User <$> _userId <*> _userUsername <*> _userPassword
