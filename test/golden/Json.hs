{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Json where

import Data.Aeson hiding (Array, Options)
import Data.Aeson.Encode.Pretty
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import IR.Transformations.Base

jsonPrettyPrint :: ToJSON a => a -> String
jsonPrettyPrint = T.unpack . decodeUtf8 . BSL.toStrict . encodePretty' defConfig

instance ToJSON Log where
  toJSON (Log lvl msg sr) = object ["type" .= show lvl, "message" .= msg, "src" .= serializeSourceRange sr]