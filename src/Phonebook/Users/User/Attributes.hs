{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Phonebook.Users.User.Attributes (Attributes (..), New) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity (Identity)
import Data.OpenApi (SchemaOptions (datatypeNameModifier), ToSchema (declareNamedSchema), defaultSchemaOptions, genericDeclareNamedSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Phonebook.Attribute (Attribute)

data Attributes f = Attributes
  { username :: Attribute f Text
  , password :: Attribute f Text
  }
  deriving (Generic)

type New = Attributes Identity

instance FromJSON New
instance ToJSON New
instance ToSchema New where
  declareNamedSchema =
    genericDeclareNamedSchema $
      defaultSchemaOptions
        { datatypeNameModifier = const "NewUser"
        }
deriving instance Show New
