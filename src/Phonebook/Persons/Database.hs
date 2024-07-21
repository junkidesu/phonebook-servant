module Phonebook.Persons.Database (allPersons, toPersonType) where

import Data.Pool (Pool, withResource)
import Database.Beam (Q, QExpr, all_, related_, runSelectReturningList, select)
import Database.Beam.Postgres (Connection, Postgres, runBeamPostgresDebug)
import Phonebook.Database (PhonebookDb (phonebookPersons, phonebookUsers), db)
import Phonebook.Persons.Database.Table
import qualified Phonebook.Persons.Person as Person
import Phonebook.Users.Database (toUserType)
import Phonebook.Users.Database.Table (User, UserT)

selectAllPersons :: Q Postgres PhonebookDb s (PersonT (QExpr Postgres s), UserT (QExpr Postgres s))
selectAllPersons = do
  person <- all_ $ phonebookPersons db
  user <- related_ (phonebookUsers db) (_personUser person)
  return (person, user)

allPersons :: Pool Connection -> IO [(Person, User)]
allPersons conns = withResource conns $ \conn ->
  runBeamPostgresDebug putStrLn conn $
    runSelectReturningList $
      select selectAllPersons

toPersonType :: (Person, User) -> Person.Person
toPersonType (person, user) =
  Person.Person
    (_personId person)
    (_personName person)
    (_personNumber person)
    (toUserType user)
