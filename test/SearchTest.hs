{-# LANGUAGE OverloadedStrings #-}
module SearchTest where

import Control.Monad
import Data.Aeson
import Network.Algolia.Search
import Test.Tasty
import Test.Tasty.HUnit

unit_sanityCheckEnv :: IO ()
unit_sanityCheckEnv = algoliaFromEnv (return ())

unit_securedApiKey :: IO ()
unit_securedApiKey =
  generateSecuredApiKey
    (ApiKey "Keyyyy")
    [("tagFilters", Just "yoyo")]
    (Just "user_42") @?=
  "MzdjZmI1YjFhM2E4YmZmOGU5NjA2Y2FhYmQ1NzM2MGVkNjZlODIxZTFlMWU4M2MwZjNhM2U0YWRiMTRhYzBkNXRhZ0ZpbHRlcnM9eW95byZ1c2VyVG9rZW49dXNlcl80Mg=="

-- Notice here how tag filter array gets encoded as json array
unit_securedApiKeyFilterArray :: IO ()
unit_securedApiKeyFilterArray =
  generateSecuredApiKey
    (ApiKey "Keyyyy")
    [("tagFilters", Just "[\"yoyo\"]")]
    (Just "user_42") @?=
  "MjE5YWUyZTQ4NDViM2QzMjRmZTU1MzJmNzAyOWJjZmU0YWFjZWFkZTI0ODI0YTM4YmZkYTlmYTYyZjA3NmVmYXRhZ0ZpbHRlcnM9JTVCJTIyeW95byUyMiU1RCZ1c2VyVG9rZW49dXNlcl80Mg=="

unit_listIndices :: IO ()
unit_listIndices = do
  void $ algoliaFromEnv $ listIndices Nothing

unit_clearIndex :: IO ()
unit_clearIndex = do
  let ix = IndexName "hs_client_test_clear_ix" :: IndexName Value
      contents = object ["body" .= ("stuff" :: String)] :: Value
  r <- algoliaFromEnv $ do
    r <- addObjectWithoutId ix contents
    waitTask ix (addObjectWithoutIdResponseTaskId r)
    ior <- clearIndex ix
    waitTask ix (indexOperationResponseTaskId ior)
    browseAllIndexContent ix
  Prelude.length (browseIndexResponseHits r) @?= 0



unit_retrieveObject :: IO ()
unit_retrieveObject = do
  let ix = IndexName "hs_client_test"
      contents = object ["body" .= ("stuff" :: String)]
  (newObj, res) <- algoliaFromEnv $ do
    newObj <- addObjectWithoutId ix contents
    waitTask ix (addObjectWithoutIdResponseTaskId newObj)
    res <- retrieveObject ix (addObjectWithoutIdResponseObjectId newObj) []
    return (newObj, res)
  res @?= Just (RetrieveObjectResponse (addObjectWithoutIdResponseObjectId newObj) contents)

-- unit_searchFacetValues

-- unit_searchSynonyms

unit_getLogs :: IO ()
unit_getLogs = do
  void $ algoliaFromEnv $ getLogs Nothing Nothing Nothing Nothing
