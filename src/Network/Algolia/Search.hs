{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Algolia.Search where
import Control.Monad.Catch
import Control.Monad.Reader
import qualified Data.Attoparsec.ByteString as A
import Data.Aeson.Parser
import Data.Aeson.Types (typeMismatch)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (toLazyByteString, intDec)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time
import Data.Aeson hiding (Result)
import Data.Scientific
import Data.Monoid
import Data.String
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types

data AlgoliaClient = AlgoliaClient
  { algoliaClientManager :: Manager
  -- , algoliaClientFallbackUrls :: [Text]
  , algoliaClientApiKey :: ByteString
  , algoliaClientApplicationId :: ByteString
  }

mkAlgoliaClient :: ByteString -> ByteString -> IO AlgoliaClient
mkAlgoliaClient k aid = do
  m <- getGlobalManager
  return $ AlgoliaClient m k aid

aesonReader :: FromJSON a => IO ByteString -> IO a
aesonReader m = do
  s <- m
  r <- go (A.parse value' s)
  case fromJSON r of
    Error e -> error e
    Success x -> return x
  where
    go (A.Fail _ _ err) = error err
    go (A.Partial f) = do
      s <- m
      go (f s)
    go (A.Done _ r) = return r

mkBaseRequest :: AlgoliaClient -> Request
mkBaseRequest AlgoliaClient{..} = defaultRequest
  { requestHeaders =
      [ ("X-Algolia-Application-Id", algoliaClientApplicationId)
      , ("X-Algolia-API-Key", algoliaClientApiKey)
      , (hContentType, "application/json; charset=UTF-8")
      ]
  , secure = True
  , port = 443
  }

mkReadRequest :: AlgoliaClient -> Request
mkReadRequest c@AlgoliaClient{..} = (mkBaseRequest c)
  { host = algoliaClientApplicationId <> "-dsn.algolia.net"
  }

mkWriteRequest :: ToJSON a => AlgoliaClient -> a -> Request
mkWriteRequest c@AlgoliaClient{..} x = (mkBaseRequest c)
  { host = algoliaClientApplicationId <> ".algolia.net"
  , requestBody = RequestBodyLBS $ encode x
  }


mkWriteRequest' :: AlgoliaClient -> Request
mkWriteRequest' c@AlgoliaClient{..} = (mkBaseRequest c)
  { host = algoliaClientApplicationId <> ".algolia.net"
  }

withApiKey :: MonadReader AlgoliaClient m => ByteString -> m a -> m a
withApiKey k = local (\a -> a { algoliaClientApiKey = k })

type Result a = forall m. (MonadReader AlgoliaClient m, MonadThrow m, MonadIO m) => m a

newtype IndexName a = IndexName { fromIndexName :: ByteString }

instance ToJSON (IndexName a) where
  toJSON = toJSON . decodeUtf8 . fromIndexName
instance FromJSON (IndexName a) where
  parseJSON (String t) = return $ IndexName $ encodeUtf8 t
  parseJSON invalid = typeMismatch "IndexName" invalid

newtype ObjectId a = ObjectId { fromObjectId :: ByteString }
  deriving (Show, Eq, IsString)

instance ToJSON (ObjectId a) where
  toJSON = toJSON . decodeUtf8 . fromObjectId
instance FromJSON (ObjectId a) where
  parseJSON (String t) = return $ ObjectId $ encodeUtf8 t
  parseJSON invalid = typeMismatch "ObjectId" invalid

newtype TaskId = TaskId { fromTaskId :: Int }
  deriving (ToJSON, FromJSON)

data IndexInfo = IndexInfo
  { indexInfoName :: Text
  , indexInfoCreatedAt :: UTCTime
  , indexInfoUpdatedAt :: UTCTime
  , indexInfoEntries :: Int
  , indexInfoDataSize :: Int
  , indexInfoFileSize :: Int
  , indexInfoLastBuildTimeS :: Int
  , indexInfoNumberOfPendingTask :: Int
  , indexInfoPendingTask :: Bool
  } deriving (Show)

instance FromJSON IndexInfo where
  parseJSON = withObject "IndexInfo" $ \r -> IndexInfo
    <$> r .: "name"
    <*> r .: "createdAt"
    <*> r .: "updatedAt"
    <*> r .: "entries"
    <*> r .: "dataSize"
    <*> r .: "fileSize"
    <*> r .: "lastBuildTimeS"
    <*> r .: "numberOfPendingTasks"
    <*> r .: "pendingTask"

data ListIndicesResponse = ListIndicesResponse
  { listIndicesResponseItems :: [IndexInfo]
  , listIndicesResponseNbPages :: Int
  } deriving (Show)

instance FromJSON ListIndicesResponse where
  parseJSON = withObject "ListIndicesResponse" $ \r -> ListIndicesResponse
    <$> r .: "items"
    <*> r .: "nbPages"

listIndices :: Maybe Int -> Result ListIndicesResponse
listIndices _ {- TODO -} = do
  c <- ask
  let r = (mkReadRequest c)
          { path = "/1/indexes"
          , method = methodGet
          }
  liftIO $ withResponse r (algoliaClientManager c) $ \resp -> do
    aesonReader $ responseBody resp

data SearchParameters = SearchParameters
  { query :: Text
  , attributesToRetrieve :: [Text]
  -- , restrictSearchableAttributes :: [Text]
{-
  , filters :: Maybe Filters
  , facets :: [Text]
  , maxValuesPerFacet :: Maybe Int
  , facetFilters :: [(Text, Text)] -- todo
  , facetingAfterDistinct :: Bool
-}
  , attributesToHighlight :: [Text]
  , attributesToSnippet :: [Text]
  , highlightPreTag :: Text
  , highlightPostTag :: Text
  , snippetEllipsisText :: Maybe Text
  , restrictHighlightAndSnippetArrays :: Bool
{-
  , page :: Int
  , hitsPerPage :: Int
  , offset :: Int
  , length :: Int
  -}
  , minWordSizeFor1Typo :: Int
  , minWordSizeFor2Typos :: Int
  {-
  , typoTolerance :: TypoTolerance
  , allowTyposOnNumericTokens :: Bool
  , ignorePlurals :: Either Bool [CountryCode]
  , disableTypoToleranceOnAttributes :: [Text]
  , aroundLatLng :: Maybe (Double, Double)
  , aroundLatLngViaIp :: Bool
  , aroundRadius :: Maybe Radius
  , aroundPrecision :: Int
  , minimumAroundRadius :: Maybe Int
  , insideBoundingBox :: [(Double, Double, Double, Double)]
  , insidePolygon :: [Vector Double]
  , queryType :: QueryType
  , removeWordsIfNoResults :: WordRemovalStrategy
  , advancedSyntax :: Bool
  , optionalWords :: [Text]
  , removeStopWords :: Either Bool [Text]
  , disableExactOnAttributes :: [Text]
  , exactOnSingleWordQuery :: ExactnessStrategy
  , alternativesAsExact :: [ExactnessStrategy] -- ?
  , distinct :: Either Bool Int
  , getRankingInfo :: Bool
  , numericFilters :: [NumericFilter]
  , tagFilters :: [TagFilter]
  , analytics :: Bool
  , analyticsTags :: [Text]
  -}
  , synonyms :: Bool
  , replaceSynonymsInHighlight :: Bool
  , minProximity :: Int
  {-
  , responseFields :: [ResponseField]
  -}
  , maxFacetHits :: Int
  , percentileComputation :: Bool
  }

defaultQuery :: SearchParameters
defaultQuery = SearchParameters
  { query = ""
  , attributesToRetrieve = ["*"]
  -- , restrictSearchableAttributes = [] -- Make null
  , attributesToHighlight = []
  , attributesToSnippet = []
  , highlightPreTag = "<em>"
  , highlightPostTag = "</em>"
  , snippetEllipsisText = Just "…"
  , restrictHighlightAndSnippetArrays = False
  , minWordSizeFor1Typo = 4
  , minWordSizeFor2Typos = 8
  , synonyms = True
  , replaceSynonymsInHighlight = True
  , minProximity = 1
  , maxFacetHits = 10
  , percentileComputation = True
  }

instance ToJSON SearchParameters where
  toJSON SearchParameters{..} = object
    [ "query" .= query
    ]

data SearchResult a = SearchResult
  { searchResultValue :: a
  , searchResultHighlightResult :: Maybe Object
  , searchResultSnippetResult :: Maybe Object
  , searchResultRankingInfo :: Maybe Object
  } deriving (Show)

instance (FromJSON a) => FromJSON (SearchResult a) where
  parseJSON = withObject "SearchResult" $ \o -> do
    h <- o .:? "_highlightResult"
    s <- o .:? "_snippetResult"
    r <- o .:? "_rankingInfo"
    v <- parseJSON $
         Object $
         H.delete "_highlightResult" $
         H.delete "_snippetResult" $
         H.delete "_rankingInfo" o
    return $ SearchResult v h s r

data SearchResults a = SearchResults
  { searchResultsHits :: [SearchResult a]
{-
  , searchResultsPage :: Int
  , searchResultsNbHits :: Int
  , searchResultsNbPages :: Int
  , searchResultsHitsPerPage :: Int
  , searchResultsProcessingtimeMs :: Int
  , searchResultsQuery :: Text
  , searchResultsParsedQuery :: Text
  , searchResultsParams :: Text
-}
  } deriving (Show)

instance FromJSON a => FromJSON (SearchResults a) where
  parseJSON = withObject "SearchResults" $ \r -> SearchResults
    <$> r .: "hits"


searchIndex :: FromJSON a => IndexName a -> SearchParameters -> Result Object -- (SearchResults a)
searchIndex ix params = do
  c <- ask
  let r = (mkReadRequest c)
          { path = "/1/indexes/" <> fromIndexName ix <> "/query"
          , method = methodPost
          , requestBody = RequestBodyLBS $ encode params
          }
  liftIO $ withResponse r (algoliaClientManager c) $ \resp -> do
    aesonReader $ responseBody resp

{-
searchMultipleIndices
-}

data DeleteIndexResponse = DeleteIndexResponse
  { deleteIndexResponseDeletedAt :: UTCTime
  , deleteIndexResponseTaskId :: TaskId
  }

instance FromJSON DeleteIndexResponse where
  parseJSON = withObject "DeleteIndexResponse" $ \o -> DeleteIndexResponse
    <$> o .: "deletedAt"
    <*> o .: "taskId"

deleteIndex :: IndexName a -> Result DeleteIndexResponse
deleteIndex ix = do
  c <- ask
  let r = (mkWriteRequest' c)
          { path = "/1/indexes/" <> fromIndexName ix
          , method = methodDelete
          }
  liftIO $ withResponse r (algoliaClientManager c) $ \resp -> do
    aesonReader $ responseBody resp


clearIndex :: IndexName a -> Result IndexOperationResponse
clearIndex ix = do
  c <- ask
  let r = (mkReadRequest c)
          { path = "/1/indexes/" <> fromIndexName ix <> "/clear"
          , method = methodPost
          }
  liftIO $ withResponse r (algoliaClientManager c) $ \resp -> do
    aesonReader $ responseBody resp

data AddObjectWithoutIdResponse a = AddObjectWithoutIdResponse
  { addObjectWithoutIdResponseCreatedAt :: UTCTime
  , addObjectWithoutIdResponseTaskId :: TaskId
  , addObjectWithoutIdResponseObjectId :: ObjectId a
  }

instance FromJSON (AddObjectWithoutIdResponse a) where
  parseJSON = withObject "AddObjectWithoutIdResponse" $ \a -> AddObjectWithoutIdResponse
    <$> a .: "createdAt"
    <*> a .: "taskID"
    <*> a .: "objectID"


-- Note: ToJSON instance must produce a JSON object
addObjectWithoutId :: ToJSON a => IndexName a -> a -> Result (AddObjectWithoutIdResponse a)
addObjectWithoutId ix val = do
  c <- ask
  let r = (mkWriteRequest c val)
          { path = "/1/indexes/" <> fromIndexName ix
          , method = methodPost
          }
  liftIO $ withResponse r (algoliaClientManager c) $ \resp -> do
    aesonReader $ responseBody resp

data AddObjectByIdResponse a = AddObjectByIdResponse
  { addObjectByIdResponseUpdatedAt :: UTCTime
  , addObjectByIdResponseTaskId :: TaskId
  , addObjectByIdResponseObjectId :: ObjectId a
  }

instance FromJSON (AddObjectByIdResponse a) where
  parseJSON = withObject "AddObjectByIdResponse" $ \a -> AddObjectByIdResponse
    <$> a .: "updatedAt"
    <*> a .: "taskID"
    <*> a .: "objectID"

-- Note: ToJSON instance must produce a JSON object
addObjectById :: ToJSON a => IndexName a -> ObjectId a -> a -> Result (AddObjectByIdResponse a)
addObjectById ix i val = do
  c <- ask
  let r = (mkWriteRequest c val)
          { path = "/1/indexes/" <> fromIndexName ix <> "/" <> fromObjectId i
          , method = methodPut
          }
  liftIO $ withResponse r (algoliaClientManager c) $ \resp -> do
    aesonReader $ responseBody resp

{-
data UpdateOp
  = Set Value
  | Increment Scientific
  | Decrement Scientific
  | Add (Either Scientific Text)
  | Remove (Either Scientific Text)
  | AddUnique (Either Scientific Text)

partiallyUpdateObject :: IndexName a -> HashMap Text UpdateOp -> Result ObjectResponse
retrieveObject :: FromJSON a => IndexName a -> ObjectId a -> [Text] -> Result (Maybe a)
retrieveObjects :: FromJSON a => [(IndexName Object, ObjectId Object, Maybe [Text])] -> Result [Object]

-}
data DeleteObjectResponse = DeleteObjectResponse
  { deleteObjectResponseDeletedAt :: UTCTime
  , deleteObjectResponseTaskId :: TaskId
  }

instance FromJSON DeleteObjectResponse where
  parseJSON = withObject "DeleteObjectResponse" $ \d -> DeleteObjectResponse
    <$> d .: "deletedAt"
    <*> d .: "taskID"

deleteObject :: IndexName a -> ObjectId a -> Result DeleteObjectResponse
deleteObject ix i = do
  c <- ask
  let r = (mkWriteRequest' c)
          { path = "/1/indexes/" <> fromIndexName ix <> "/" <> fromObjectId i
          , method = methodDelete
          }
  liftIO $ withResponse r (algoliaClientManager c) $ \resp -> do
    aesonReader $ responseBody resp

{-
batch
batchMultipleIndices
browseAllIndexContent
changeIndexSettings
-}

data IndexOperation = MoveIndex | CopyIndex
data IndexOperationResponse = IndexOperationResponse
  { indexOperationResponseUpdatedAt :: UTCTime
  , indexOperationResponseTaskId :: TaskId
  }

instance FromJSON IndexOperationResponse where
  parseJSON = withObject "IndexOperationResponse" $ \a -> IndexOperationResponse
    <$> a .: "updatedAt"
    <*> a .: "taskID"

copyOrMoveIndex
  :: IndexOperation
  -> IndexName a
  -> IndexName a
  -> Result IndexOperationResponse
copyOrMoveIndex op from to = do
  c <- ask
  let r = (mkWriteRequest c $ object
            [ "operation" .= case op of
                               MoveIndex -> "move" :: Text
                               CopyIndex -> "copy"
            , "destination" .= to
            ])
          { path = "/1/indexes/" <> fromIndexName from <> "/operation"
          , method = methodPost
          }
  liftIO $ withResponse r (algoliaClientManager c) $ \resp -> do
    aesonReader $ responseBody resp

data TaskStatus = Published | NotPublished
  deriving (Show)

instance FromJSON TaskStatus where
  parseJSON = withText "TaskStatus" $ \t -> case t of
    "published" -> pure Published
    "notPublished" -> pure NotPublished
    _ -> fail ("Invalid TaskStatus value " ++ show t)

data TaskStatusResult = TaskStatusResult
  { taskStatusResultStatus :: TaskStatus
  , taskStatusResultPendingTask :: Bool
  } deriving (Show)

instance FromJSON TaskStatusResult where
  parseJSON = withObject "TaskStatusResult" $ \r -> TaskStatusResult
    <$> r .: "status"
    <*> r .: "pendingTask"

getTaskStatus :: IndexName a -> TaskId -> Result TaskStatusResult
getTaskStatus ix t = do
  c <- ask
  let r = (mkReadRequest c)
          { path = "/1/indexes/" <> fromIndexName ix <> "/task/" <> (toStrict $ toLazyByteString $ intDec $ fromTaskId t)
          , method = methodPost
          }
  liftIO $ withResponse r (algoliaClientManager c) $ \resp -> do
    aesonReader $ responseBody resp
{-
addIndexApiKey
updateIndexApiKey
listIndexApiKeys
listIndexApiKeysForAllIndices
retriveIndexApiKey
deleteIndexApiKey
searchFacetValues
newtype SynonymId = SynonymId { fromSynonymId :: Text }
data Correction = Correction
  { correctionWord :: Text
  , correctionCorrections :: [Text]
  }

data Synonym = MultiWaySynonym [Text]
             | OneWaySynonym Text [Text]
             | AlternativeCorrection1 Correction
             | AlternativeCorrection2 Correction
             | Placeholder Text [Text]

setSynonym
  :: IndexName a
  -> SynonymId
  -> Bool -- ^ Replicate the new/updated synonym set to all replica indices.
  -> Synonym
  -> Result ()


batchSynonyms
getSynonym
deleteAllSynonyms

deleteSynonymSet
  :: IndexName a
  -> SynonymId
  -> Bool -- ^ delete the synonyms set in all replica indices as well
  -> Result ()

-}

data SynonymType
  = Synonym
  | OneWaySynonym
  | AltCorrection1
  | AltCorrection2
  | Placeholder

synonymTypeName :: SynonymType -> Text
synonymTypeName Synonym = "synonym"
synonymTypeName OneWaySynonym = "onewaysynonym"
synonymTypeName AltCorrection1 = "altcorrection1"
synonymTypeName AltCorrection2 = "altcorrection2"
synonymTypeName Placeholder = "placeholder"

data SynonymSearch = SynonymSearch
  { synonymSearchQuery :: Maybe Text
  , synonymSearchType :: [SynonymType]
  , synonymSearchPage :: Maybe Int
  , synonymSearchHitsPerPage :: Maybe Int
  }

instance ToJSON SynonymSearch where
  toJSON SynonymSearch{..} = object
    [ "query" .= synonymSearchQuery
    , "type" .= case synonymSearchType of
        [] -> Null
        ts -> String $ T.intercalate "," $ map synonymTypeName ts
    , "page" .= synonymSearchPage
    , "hitsPerPage" .= synonymSearchHitsPerPage
    ]

type SynonymSearchResponse = Object

searchSynonyms :: IndexName a -> SynonymSearch -> Result SynonymSearchResponse
searchSynonyms ix params = do
  c <- ask
  let r = (mkReadRequest c)
          { path = "/1/indexes/" <> fromIndexName ix <> "/synonyms/search"
          , method = methodPost
          , requestBody = RequestBodyLBS $ encode params
          }
  liftIO $ withResponse r (algoliaClientManager c) $ \resp -> do
    aesonReader $ responseBody resp

{-
addApiKey
updateApiKey
listApiKeys
getApiKey
deleteApiKey
getLogs

-}
