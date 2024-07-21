module Phonebook.Users.Database (
  allUsers,
  userByUsername,
  createUser,
  toUserType,
) where

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

insertUser :: Attributes.NewUser -> SqlInsertValues Postgres (UserT (QExpr Postgres s))
insertUser nu =
  insertExpressions
    [ User
        default_
        (val_ . Attributes.username $ nu)
        (val_ . Attributes.password $ nu)
    ]

allUsers :: Pool Connection -> IO [User]
allUsers conns = withResource conns $ \conn -> do
  runBeamPostgres conn $ runSelectReturningList $ select selectAllUsers

userByUsername :: Pool Connection -> Text -> IO (Maybe User)
userByUsername conns username = withResource conns $ \conn -> do
  runBeamPostgres conn $
    runSelectReturningFirst $
      select $
        selectUserByUsername username

createUser :: Pool Connection -> Attributes.NewUser -> IO User
createUser conns nu = withResource conns $ \conn -> do
  [insertedUser] <-
    runBeamPostgres conn $
      runInsertReturningList $
        insert (phonebookUsers db) $
          insertUser nu
  return insertedUser

toUserType :: User -> User.User
toUserType = User.User <$> _userId <*> _userUsername <*> _userPassword
