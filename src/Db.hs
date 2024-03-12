{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Db where

import qualified Data.ByteString.UTF8 as BSU
import Data.Pool
import Database.PostgreSQL.Simple
import System.Environment (getEnv)

connectToDb :: IO (Pool Connection)
connectToDb = do
  dbConnString <- BSU.fromString <$> getEnv "DATABASE_URL"
  newPool . setNumStripes (Just 2) $ defaultPoolConfig (connectPostgreSQL dbConnString) close 60 10
