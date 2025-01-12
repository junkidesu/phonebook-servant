{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Persons.Web.Avatar.Update where

import Servant
import Servant.Multipart

type Endpoint = Post '[]
