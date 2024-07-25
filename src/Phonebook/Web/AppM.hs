module Phonebook.Web.AppM (AppM) where

import Control.Monad.Trans.Reader (ReaderT)
import Phonebook.Web.Environment (Environment)
import Servant (Handler)

type AppM = ReaderT Environment Handler
