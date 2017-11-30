{-# LANGUAGE OverloadedStrings #-}
module Network.Algolia.Vault where

import Data.Aeson
import Data.Aeson.IP
import Data.IP
import Data.Text (Text)
import Data.Time

data Source = Source
  { sourceIPRange :: IPRange
  , sourceDescription :: Maybe Text
  }


instance ToJSON Source where
  toJSON src = object $ case sourceDescription src of
    Nothing -> ["source" .= sourceIPRange src ]
    Just desc -> ["source" .= sourceIPRange src, "description" .= desc]

instance FromJSON Source where
  parseJSON = withObject "Source" $ \o ->
    Source <$>
    o .: "source" <*>
    o .:? "description"

-- listAllowedSources

-- replaceAllowedSources

-- appendAllowedSource

-- deleteAllowedSource
-- "deletedAt" :: UTCTime
