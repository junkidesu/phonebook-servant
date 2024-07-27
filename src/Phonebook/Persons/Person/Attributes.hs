{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Phonebook.Persons.Person.Attributes (Attributes (..), New, Edit) where

import Data.Text (Text)

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity (Identity)
import Data.OpenApi (SchemaOptions (datatypeNameModifier), ToSchema (declareNamedSchema), defaultSchemaOptions, genericDeclareNamedSchema)
import GHC.Generics (Generic)
import Phonebook.Attribute

data Attributes f = Attributes
  { name :: Attribute f Text
  , number :: Attribute f Text
  }
  deriving (Generic)

type New = Attributes Identity
type Edit = Attributes Maybe

deriving instance Show New
instance FromJSON New
instance ToJSON New
instance ToSchema New where
  declareNamedSchema =
    genericDeclareNamedSchema $
      defaultSchemaOptions
        { datatypeNameModifier = const "NewPerson"
        }

deriving instance Show Edit
instance FromJSON Edit
instance ToJSON Edit
instance ToSchema Edit where
  declareNamedSchema =
    genericDeclareNamedSchema $
      defaultSchemaOptions
        { datatypeNameModifier = const "EditPerson"
        }
