module Phonebook.Users.Database (
  allUsers,
  userByUsername,
  userById,
  createUser,
  deleteUser,
  toUserType,
) where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam hiding (liftIO)
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList))
import Database.Beam.Postgres (Postgres, runBeamPostgres)
import Phonebook.Database (PhonebookDb (phonebookUsers), db, withDatabaseConnection)
import Phonebook.Users.Database.Table
import qualified Phonebook.Users.User as User
import qualified Phonebook.Users.User.Attributes as Attributes
import Phonebook.Web.AppM (AppM)

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

allUsers :: AppM [User]
allUsers = do
  withDatabaseConnection $ \conn -> do
    runBeamPostgres conn $
      runSelectReturningList $
        select selectAllUsers

userByUsername :: Text -> AppM (Maybe User)
userByUsername username = do
  withDatabaseConnection $ \conn -> do
    runBeamPostgres conn
      . runSelectReturningFirst
      . select
      . selectUserByUsername
      $ username

userById :: Int32 -> AppM (Maybe User)
userById userId = do
  withDatabaseConnection $ \conn ->
    runBeamPostgres conn
      . runSelectReturningFirst
      . select
      . selectUserById
      $ userId

createUser :: Attributes.New -> AppM User
createUser nu = do
  withDatabaseConnection $ \conn -> do
    [insertedUser] <-
      runBeamPostgres conn
        . runInsertReturningList
        . insert (phonebookUsers db)
        $ insertExpressions
          [ User
              default_
              (val_ . Attributes.username $ nu)
              (val_ . Attributes.password $ nu)
          ]
    return insertedUser

deleteUser :: Int32 -> AppM ()
deleteUser userId = do
  withDatabaseConnection $ \conn -> do
    runBeamPostgres conn
      . runDelete
      $ delete
        (phonebookUsers db)
        (\user -> pk user ==. val_ (UserId userId))

toUserType :: User -> User.User
toUserType = User.User <$> _userId <*> _userUsername <*> _userPassword
