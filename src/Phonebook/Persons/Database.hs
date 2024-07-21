module Phonebook.Persons.Database (allPersons, personById, toPersonType) where

import Data.Int (Int32)
import Data.Pool (Pool, withResource)
import Database.Beam
import Database.Beam.Postgres (Connection, Postgres, runBeamPostgresDebug)
import Phonebook.Database (PhonebookDb (phonebookPersons, phonebookUsers), db)
import Phonebook.Persons.Database.Table
import qualified Phonebook.Persons.Person as Person
import Phonebook.Users.Database (toUserType)
import Phonebook.Users.Database.Table (User, UserT)

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

allPersons :: Pool Connection -> IO [(Person, User)]
allPersons conns = withResource conns $ \conn ->
  runBeamPostgresDebug putStrLn conn $
    runSelectReturningList $
      select selectAllPersons

personById :: Pool Connection -> Int32 -> IO (Maybe (Person, User))
personById conns personId = withResource conns $ \conn ->
  runBeamPostgresDebug putStrLn conn $
    runSelectReturningFirst $
      select $
        selectPersonById personId

toPersonType :: (Person, User) -> Person.Person
toPersonType (person, user) =
  Person.Person
    (_personId person)
    (_personName person)
    (_personNumber person)
    (toUserType user)
