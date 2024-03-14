{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Util.ErrorFormatters (customFormatters) where

import Data.Aeson
import Data.String.Conversions (cs)
import Servant
import Servant.API.ContentTypes

customFormatter :: ErrorFormatter
customFormatter tr req err =
  let
    value = object ["combinator" .= show tr, "error" .= err]
    accH = getAcceptHeader req
   in
    case handleAcceptH (Proxy :: Proxy '[JSON]) accH value of
      Nothing -> err400{errBody = cs err}
      Just (ctypeH, body) ->
        err400
          { errBody = body
          , errHeaders = [("Content-Type", cs ctypeH)]
          }

authFormatter :: ErrorFormatter
authFormatter tr req err =
  let
    value = object ["combinator" .= show tr, "error" .= err]
    accH = getAcceptHeader req
   in
    case handleAcceptH (Proxy :: Proxy '[JSON]) accH value of
      Nothing -> err400{errBody = cs err}
      Just (ctypeH, body) ->
        err400
          { errBody = body
          , errHeaders = [("Content-Type", cs ctypeH)]
          }

customFormatters :: ErrorFormatters
customFormatters = defaultErrorFormatters{bodyParserErrorFormatter = customFormatter, urlParseErrorFormatter = customFormatter}
