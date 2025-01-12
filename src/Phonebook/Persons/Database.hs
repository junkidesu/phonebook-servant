module Phonebook.Persons.Database (
  allPersons,
  personById,
  createPerson,
  deletePerson,
  updatePerson,
  updateAvatar,
  toPersonType,
) where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList), MonadBeamUpdateReturning (runUpdateReturningList))
import Database.Beam.Postgres
import Phonebook.Database (PhonebookDb (phonebookPersons, phonebookUsers), db, withDatabaseConnection)
import Phonebook.Persons.Database.Table
import qualified Phonebook.Persons.Person as Person
import qualified Phonebook.Persons.Person.Attributes as Attributes
import Phonebook.Users.Database (toUserType)
import Phonebook.Users.Database.Table (PrimaryKey (UserId), User, UserT)
import Phonebook.Web.AppM (AppM)

type PersonQuery s = Q Postgres PhonebookDb s (PersonT (QExpr Postgres s), UserT (QExpr Postgres s))

selectAllPersons :: PersonQuery s
selectAllPersons = do
  person <- all_ $ phonebookPersons db
  user <- related_ (phonebookUsers db) (_personUser person)
  return (person, user)

selectPersonById :: Int32 -> PersonQuery s
selectPersonById personId =
  filter_
    (\(person, _) -> _personId person ==. val_ personId)
    selectAllPersons

allPersons :: AppM [(Person, User)]
allPersons = do
  withDatabaseConnection $ \conn ->
    runBeamPostgres conn $
      runSelectReturningList $
        select selectAllPersons

personById :: Int32 -> AppM (Maybe (Person, User))
personById personId = do
  withDatabaseConnection $ \conn ->
    runBeamPostgres conn $
      runSelectReturningFirst $
        select $
          selectPersonById personId

createPerson :: Attributes.New -> Int32 -> AppM (Person, User)
createPerson np userId = do
  withDatabaseConnection $ \conn -> do
    runBeamPostgres conn $ do
      [person] <-
        runInsertReturningList $
          insert (phonebookPersons db) $
            insertExpressions
              [ Person
                  default_
                  (val_ $ Attributes.name np)
                  (val_ $ Attributes.number np)
                  (val_ Nothing)
                  (val_ $ UserId userId)
              ]
      [user] <-
        runSelectReturningList $
          select $
            related_ (phonebookUsers db) (val_ $ _personUser person)

      return (person, user)

deletePerson :: Int32 -> AppM ()
deletePerson userId = do
  withDatabaseConnection $ \conn ->
    runBeamPostgres conn $
      runDelete $
        delete (phonebookPersons db) (\person -> _personId person ==. val_ userId)

updatePerson :: Int32 -> Attributes.Edit -> AppM (Person, User)
updatePerson personId ep = do
  withDatabaseConnection $ \conn -> do
    runBeamPostgres conn $ do
      [person] <-
        runUpdateReturningList $
          update
            (phonebookPersons db)
            ( \person ->
                mconcat
                  [ maybe mempty (\n -> _personName person <-. val_ n) (Attributes.name ep)
                  , maybe mempty (\n -> _personNumber person <-. val_ n) (Attributes.number ep)
                  ]
            )
            (\person -> _personId person ==. val_ personId)

      [user] <-
        runSelectReturningList $
          select $
            related_ (phonebookUsers db) (val_ $ _personUser person)

      return (person, user)

updateAvatar :: Int32 -> Text -> AppM (Person, User)
updateAvatar personId avatarUrl = withDatabaseConnection $
  \conn -> do
    runBeamPostgres conn $ do
      [person] <-
        runUpdateReturningList $
          update
            (phonebookPersons db)
            (\person -> mconcat [_personAvatar person <-. val_ (Just avatarUrl)])
            (\person -> _personId person ==. val_ personId)
      [user] <-
        runSelectReturningList $
          select $
            related_ (phonebookUsers db) (val_ $ _personUser person)
      return (person, user)

toPersonType :: (Person, User) -> Person.Person
toPersonType (person, user) =
  Person.Person
    (_personId person)
    (_personName person)
    (_personNumber person)
    (_personAvatar person)
    (toUserType user)
