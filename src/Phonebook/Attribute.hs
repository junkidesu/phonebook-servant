{-# LANGUAGE TypeFamilies #-}

module Phonebook.Attribute (Attribute) where

import Data.Functor.Identity (Identity)

type family Attribute f a where
  Attribute Identity a = a
  Attribute Maybe a = Maybe a
