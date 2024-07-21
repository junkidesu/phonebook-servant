{-# LANGUAGE DataKinds #-}

module Phonebook.Web.JWTAuth where

import qualified Phonebook.Users.Web.Login.User as Login
import Servant.Auth

type JWTAuth = Auth '[JWT] Login.User
