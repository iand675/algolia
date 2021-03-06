{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}

{- |
Algolia is not based on traditional search solutions like Lucene or Solr. Algolia was built from the ground up specifically for searching through semi-structured data to power user-facing search.

Speed
In order to provide the best user experience, we focus on performance - milliseconds matter, and we developed an engine capable of delivering results in a few milliseconds, fast enough to power a seamless, “as-you-type” experience - the de facto consumer-grade search experience.

Our ability to achieve unparalleled speed relies on a multitude of factors, many of which are outlined in this blog post. For instance, the Algolia engine handles the vast majority of computation at indexing time, as opposed to at query time. Beyond this, we control the full stack end-to-end. We’ve obsessed over every detail, from obtaining high quality infrastructure (bare-metal servers!) to crafting our own Ubuntu-based OS painstakingly modified for search engine performance.

Beyond speed, we focus on providing all the features necessary to build a full-fledged search experience out-of-the-box: prefix search, typo-tolerance, faceting, highlighting, and more.

Relevance

Performance is important; however, in order for search to be successful, results need to be relevant to the user. Varying from the traditional TF-IDF approach to relevancy, we built a modern tie-breaking algorithm tuned specifically for semi-structured data. Rather than calculating one “score” per result and sorting based solely off a magic number, Algolia calculates N scores and then applies N successive sorting. In layman’s terms, this means “buckets” of results are sorted by a cascade of varying criteria. When matches occur in the first criterion, the second criterion is considered to help break the tie, and so on in succession. This approach is meant to mimic how our brains naturally search for information - and as a consequence, is far easier to debug.

The Algolia engine provides various criteria for building stellar textual relevancy. On top of these, additional rules can be set to further hone the relevancy for a specific dataset. For example, a numeric “popularity” attribute could be leveraged as a potential tie-breaker rule to help more popular items rank higher. By enabling business relevance metrics to be added at will and via API, this forms the foundation for handling advanced search experiences like personalization, merchandising and more.
-}
module Network.Algolia.Search
  ( mkAlgoliaClient
  , AlgoliaClient
  , ApiKey(..)
  , ApplicationId(..)
  , simpleAlgolia
  , algoliaFromEnv
  , withApiKey
  , Result
  , Reconstrain(..)
  , IndexName(..)
  , ObjectId(..)
  , TaskId(..)
  , IndexInfo(..)
  , ListIndicesResponse(..)
  , listIndices
  , SearchParameters(..)
  , defaultQuery
  , SearchResult(..)
  , generateSecuredApiKey
  , FacetStat(..)
  , SearchResults(..)
  , searchIndex
  , MultiIndexSearchStrategy(..)
  , searchMultipleIndices
  , DeleteIndexResponse(..)
  , deleteIndex
  , clearIndex
  , AddObjectWithoutIdResponse(..)
  , addObjectWithoutId
  , AddObjectByIdResponse(..)
  , addObjectById
  , UpdateOp(..)
  , ObjectResponse(..)
  , partiallyUpdateObject
  , RetrieveObjectResponse(..)
  , retrieveObject
  , retrieveObjects
  , DeleteObjectResponse(..)
  , deleteObject
  , BatchOp(..)
  , BatchResponse(..)
  , batch
  , BatchMultipleIndicesResponse(..)
  , batchMultipleIndices
  , BrowseIndexResponse(..)
  , browseAllIndexContent
  -- , changeIndexSettings
  , IndexOperation(..)
  , IndexOperationResponse(..)
  , copyOrMoveIndex
  , TaskStatus(..)
  , TaskStatusResult(..)
  , waitTask
  , getTaskStatus
  -- , addIndexApiKey
  -- , updateIndexApiKey
  -- , listIndexApiKeys
  -- , listIndexApiKeysForAllIndices
  -- , retrieveIndexApiKey
  -- , deleteIndexApiKey
  , FacetName(..)
  , FacetQuery(..)
  , FacetHit(..)
  , FacetHits(..)
  , searchFacetValues
  , SynonymId(..)
  , Synonym(..)
  , Correction(..)
  -- , setSynonym
  -- , batchSynonyms
  -- , getSynonym
  -- , deleteAllSynonyms
  -- , deleteSynonymSet
  , SynonymType(..)
  , SynonymSearch(..)
  , SynonymSearchResponse(..)
  , searchSynonyms
  -- , addApiKey
  -- , updateApiKey
  -- , listApiKeys
  -- , getApiKey
  -- , deleteApiKey
  , LogType(..)
  , LogsResponse(..)
  , getLogs
  , AlgoliaError(..)
  ) where
import Control.Applicative
import Control.Exception
import Control.Retry
import Control.Monad.Catch
import Control.Monad.Reader
import Crypto.Hash.Algorithms
import Crypto.MAC.HMAC
import qualified Data.Attoparsec.ByteString as A
import Data.Aeson.Parser
import Data.ByteArray.Encoding
import Data.ByteString.Char8 (ByteString, unpack, pack)
import qualified Data.ByteString.Lazy as L
import Data.Coerce
import Data.Has
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Time
import Data.Typeable
import Data.Vector (Vector)
import Data.Aeson hiding (Result)
import Data.Scientific
import Data.Monoid
import Data.String
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Network.HTTP.Types.QueryLike
import Network.URI.Template
import qualified Network.URI.Template as URI
import System.Environment

newtype ApiKey = ApiKey { fromApiKey :: ByteString }
newtype ApplicationId = ApplicationId { fromApplicationId :: ByteString}

data AlgoliaClient = AlgoliaClient
  { algoliaClientFallbackUrls :: Vector Text
  , algoliaClientApiKey :: ApiKey
  , algoliaClientApplicationId :: ApplicationId
  }

-- | Make an algolia client from the provided API key and application id.
mkAlgoliaClient :: ApiKey -> ApplicationId -> AlgoliaClient
mkAlgoliaClient k aid = AlgoliaClient mempty k aid

-- | Given a data type @c@ that implements @Has AlgoliaClient c@, perform an algolia request.
simpleAlgolia :: (MonadIO m, Has AlgoliaClient c) => c -> ReaderT c m a -> m a
simpleAlgolia = flip runReaderT

-- | Create an algolia client from @ALGOLIA_APP_ID@ and @ALGOLIA_KEY@ environment variables.
algoliaFromEnv :: (MonadIO m) => ReaderT AlgoliaClient m a -> m a
algoliaFromEnv m = do
  k <- liftIO $ getEnv "ALGOLIA_KEY"
  i <- liftIO $ getEnv "ALGOLIA_APP_ID"
  simpleAlgolia (mkAlgoliaClient (ApiKey $ pack k) (ApplicationId $ pack i)) m

data AlgoliaError
  = JsonParseError String -- ^ The response was a JSON value, but the library does not know how to handle it properly.
  | NonConformingResult Request (Response ()) Value String -- ^ The response was not JSON.
  | ToJsonInstanceMustProduceAnObject -- ^ A JSON object was expected as the response, but it was some other JSON type.
  deriving (Show, Typeable)

instance Exception AlgoliaError

aesonReader :: (MonadIO m, MonadThrow m, FromJSON a) => Request -> Response BodyReader -> m a
aesonReader req m = do
  s <- liftIO (responseBody m)
  r <- go (A.parse value' s)
  case fromJSON r of
    Error e -> throwM $ NonConformingResult req (m { responseBody = () }) r e
    Success x -> return x
  where
    go (A.Fail _ _ err) = throwM $ JsonParseError err
    go (A.Partial f) = do
      s <- liftIO (responseBody m)
      go (f s)
    go (A.Done _ r) = return r

mkBaseRequest :: AlgoliaClient -> Request
mkBaseRequest AlgoliaClient{..} = defaultRequest
  { requestHeaders = -- ("X-Algolia-UserToken"), ("X-Algolia-Agent", _), ("X-Algolia-TagFilters", _)
      [ ("X-Algolia-Application-Id", fromApplicationId algoliaClientApplicationId)
      , ("X-Algolia-API-Key", fromApiKey algoliaClientApiKey)
      , (hContentType, "application/json; charset=UTF-8")
      ]
  , secure = True
  , port = 443
  }

mkReadRequest :: AlgoliaClient -> Request
mkReadRequest c@AlgoliaClient{..} = (mkBaseRequest c)
  { host = [uri|{strHost}-dsn.algolia.net|]
  }
  where
    strHost = URI.String $ unpack $ fromApplicationId algoliaClientApplicationId

mkWriteRequest :: ToJSON a => AlgoliaClient -> a -> Request
mkWriteRequest c@AlgoliaClient{..} x = (mkBaseRequest c)
  { host = [uri|{strHost}.algolia.net|]
  , requestBody = RequestBodyLBS $ encode x
  }
  where
    strHost = URI.String $ unpack $ fromApplicationId algoliaClientApplicationId


mkWriteRequest' :: AlgoliaClient -> Request
mkWriteRequest' c@AlgoliaClient{..} = (mkBaseRequest c)
  { host = [uri|{strHost}.algolia.net}|]
  }
  where
    strHost = URI.String $ unpack $ fromApplicationId algoliaClientApplicationId

withApiKey :: MonadReader AlgoliaClient m => ApiKey -> m a -> m a
withApiKey k = local (\a -> a { algoliaClientApiKey = k })

type Result a = forall c m. (Has AlgoliaClient c, MonadReader c m, MonadThrow m, MonadIO m) => m a

newtype IndexName a = IndexName { fromIndexName :: ByteString }
  deriving (Show, Eq, Hashable)

instance ToJSON (IndexName a) where
  toJSON = toJSON . decodeUtf8 . fromIndexName

instance FromJSON (IndexName a) where
  parseJSON = withText "IndexName" (return . IndexName . encodeUtf8)

instance FromJSONKey (IndexName a)

instance ToTemplateValue (IndexName a) where
  toTemplateValue = Single . unpack . fromIndexName

newtype ObjectId a = ObjectId { fromObjectId :: ByteString }
  deriving (Show, Eq, IsString)

instance ToJSON (ObjectId a) where
  toJSON = toJSON . decodeUtf8 . fromObjectId

instance FromJSON (ObjectId a) where
  parseJSON = withText "ObjectId" (return . ObjectId . encodeUtf8)

instance ToTemplateValue (ObjectId a) where
  toTemplateValue = Single . unpack . fromObjectId

newtype TaskId = TaskId { fromTaskId :: Int }
  deriving (Show, Eq, ToJSON, FromJSON)

instance ToTemplateValue TaskId where
  toTemplateValue = toTemplateValue . fromTaskId

-- | Convert a coercible type f
class Reconstrain f where
  reconstrain :: f a -> f b
  default reconstrain :: Coercible (f a) (f b) => f a -> f b
  reconstrain = coerce

instance Reconstrain ObjectId
instance Reconstrain IndexName
instance Reconstrain Proxy where
  reconstrain _ = Proxy

data IndexInfo = IndexInfo
  { indexInfoName :: IndexName Object
  -- , indexInfoCreatedAt :: UTCTime
  -- , indexInfoUpdatedAt :: UTCTime
  , indexInfoEntries :: Int
  , indexInfoDataSize :: Int
  , indexInfoFileSize :: Int
  , indexInfoLastBuildTimeS :: Int
  , indexInfoNumberOfPendingTask :: Int
  , indexInfoPendingTask :: Bool
  } deriving (Show, Eq)

instance FromJSON IndexInfo where
  parseJSON = withObject "IndexInfo" $ \r -> IndexInfo
    <$> r .: "name"
    -- <*> r .: "createdAt"
    -- <*> r .: "updatedAt"
    <*> r .: "entries"
    <*> r .: "dataSize"
    <*> r .: "fileSize"
    <*> r .: "lastBuildTimeS"
    <*> r .: "numberOfPendingTasks"
    <*> r .: "pendingTask"

data ListIndicesResponse = ListIndicesResponse
  { listIndicesResponseItems :: [IndexInfo]
  , listIndicesResponseNbPages :: Int
  } deriving (Show, Eq)

instance FromJSON ListIndicesResponse where
  parseJSON = withObject "ListIndicesResponse" $ \r -> ListIndicesResponse
    <$> r .: "items"
    <*> r .: "nbPages"


-- | List existing indexes.
listIndices
  :: Maybe Int -- ^ Requested page (zero-based). When specified, will retrieve a specific page; the page size is implicitly set to 100. When @Nothing@, will retrieve all indices (no pagination).
  -> Result ListIndicesResponse
listIndices _ {- TODO -} = do
  c <- getter <$> ask
  m <- liftIO getGlobalManager
  let r = (mkReadRequest c)
          { path = [uri|/1/indexes|]
          , method = methodGet
          }
  liftIO $ withResponse r m $ \resp -> do
    aesonReader r resp

data SearchParameters = SearchParameters
  { query :: Text
  , attributesToRetrieve :: [Text]
  -- , restrictSearchableAttributes :: [Text]
{-
  , filters :: Maybe Filters
-}
  , facets :: [Text]
  , maxValuesPerFacet :: Maybe Int
{-
  , facetFilters :: [(Text, Text)] -- todo
  , facetingAfterDistinct :: Bool
-}
  , attributesToHighlight :: [Text]
  , attributesToSnippet :: [Text]
  , highlightPreTag :: Text
  , highlightPostTag :: Text
  , snippetEllipsisText :: Maybe Text
  , restrictHighlightAndSnippetArrays :: Bool
  , page :: Int
  , hitsPerPage :: Int
  , offset :: Maybe Int
  , length :: Maybe Int
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
  -}
  , analytics :: Bool
  , analyticsTags :: [Text]
  , synonyms :: Bool
  , replaceSynonymsInHighlight :: Bool
  , minProximity :: Int
  {-
  , responseFields :: [ResponseField]
  -}
  , maxFacetHits :: Int
  , percentileComputation :: Bool
  } deriving (Show)

defaultQuery :: SearchParameters
defaultQuery = SearchParameters
  { query = ""
  , attributesToRetrieve = ["*"]
  , facets = []
  , maxValuesPerFacet = Nothing
  -- , restrictSearchableAttributes = [] -- Make null
  , page = 0
  , hitsPerPage = 20
  , offset = Nothing
  , length = Nothing
  , attributesToHighlight = []
  , attributesToSnippet = []
  , highlightPreTag = "<em>"
  , highlightPostTag = "</em>"
  , snippetEllipsisText = Just "…"
  , restrictHighlightAndSnippetArrays = False
  , minWordSizeFor1Typo = 4
  , minWordSizeFor2Typos = 8
  , analytics = True
  , analyticsTags = []
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
  parseJSON =
    withObject "SearchResult" $ \o -> do
      h <- o .:? "_highlightResult"
      s <- o .:? "_snippetResult"
      r <- o .:? "_rankingInfo"
      v <-
        parseJSON $
        Object $
        H.delete "_highlightResult" $
        H.delete "_snippetResult" $ H.delete "_rankingInfo" o
      return $ SearchResult v h s r

data FacetStat = FacetStat
  { facetStatMin :: Scientific
  , facetStatMax :: Scientific
  , facetStatAvg :: Scientific
  , facetStatSum :: Scientific
  } deriving (Show)

instance FromJSON FacetStat where
  parseJSON =
    withObject "FacetStat" $ \o ->
      FacetStat <$> o .: "min" <*> o .: "max" <*> o .: "avg" <*> o .: "sum"

data SearchResults a = SearchResults
  { searchResultsHits :: [SearchResult a]
  , searchResultsPage :: Int
  , searchResultsNbHits :: Int
  , searchResultsNbPages :: Int
  , searchResultsHitsPerPage :: Int
  , searchResultsProcessingtimeMs :: Int
  , searchResultsQuery :: Text
  , searchResultsParsedQuery :: Maybe Text
  , searchResultsParams :: Text
  , searchResultsExhaustiveNbHits :: Bool
  , searchResultsQueryAfterRemoval :: Maybe Text
  , searchResultsMessage :: Maybe Text
  , searchResultsAroundLatLng :: Maybe Text -- TODO better type
  , searchResultsAutomaticRadius :: Maybe Text -- TODO better type
  , searchResultsServerUsed :: Maybe Text
  , searchResultsFacets :: Maybe (HashMap FacetName Int)
  , searchResultsFacetsStats :: Maybe (HashMap FacetName FacetStat)
  , searchResultsExhaustiveFacetCount :: Maybe Bool
  } deriving (Show)

instance FromJSON a => FromJSON (SearchResults a) where
  parseJSON = withObject "SearchResults" $ \r -> SearchResults
    <$> r .: "hits"
    <*> r .: "page"
    <*> r .: "nbHits"
    <*> r .: "nbPages"
    <*> r .: "hitsPerPage"
    <*> r .: "processingTimeMS"
    <*> r .: "query"
    <*> r .:? "parsedQuery"
    <*> r .: "params"
    <*> r .: "exhaustiveNbHits"
    <*> r .:? "queryAfterRemoval"
    <*> r .:? "message"
    <*> r .:? "aroundLatLng"
    <*> r .:? "automaticRadius"
    <*> r .:? "serverUsed"
    <*> r .:? "facets"
    <*> r .:? "facets_stats"
    <*> r .:? "exhaustiveFacetsCount"

-- | Return objects that match the query.
--
-- You can find the list of parameters that you can use in the POST body in the Search Parameters section.
--
-- Alternatively, parameters may be specified as a URL-encoded query string inside the params attribute.
searchIndex :: FromJSON a => IndexName a -> SearchParameters -> Result {- Object -} (SearchResults a)
searchIndex ix params = do
  c <- getter <$> ask
  m <- liftIO getGlobalManager
  let r = (mkReadRequest c)
          { path = [uri|/1/indexes/{ix}/query|]
          , method = methodPost
          , requestBody = RequestBodyLBS $ encode params
          }
  liftIO $ withResponse r m $ \resp -> do
    aesonReader r resp

data MultiIndexSearchStrategy
  = None
  | StopIfEnoughMatches
  deriving (Show, Eq)

instance ToJSON MultiIndexSearchStrategy where
  toJSON strat =
    case strat of
      None -> "none"
      StopIfEnoughMatches -> "stopIfEnoughMatches"

-- TODO gotta actually pull the results out of the output, right?
searchMultipleIndices :: [(IndexName Object, Query)] -> Maybe MultiIndexSearchStrategy -> Result [SearchResults Object]
searchMultipleIndices searches strat = do
  c <- getter <$> ask
  m <- liftIO getGlobalManager
  let r = (mkReadRequest c)
          { path = [uri|/1/indexes/*/query|]
          , method = methodPost
          , requestBody = RequestBodyLBS $ encode $ object
            [ "requests" .= map (\(ix, o) -> object ["indexName" .= ix, "params" .= decodeUtf8 (renderQuery False o)]) searches
            , "strategy" .= fromMaybe None strat
            ]
          }
  liftIO $ withResponse r m $ \resp -> do
    aesonReader r resp

data DeleteIndexResponse = DeleteIndexResponse
  { deleteIndexResponseDeletedAt :: UTCTime
  , deleteIndexResponseTaskId :: TaskId
  } deriving (Show)

instance FromJSON DeleteIndexResponse where
  parseJSON = withObject "DeleteIndexResponse" $ \o -> DeleteIndexResponse
    <$> o .: "deletedAt"
    <*> o .: "taskId"

-- | Delete an existing index.
deleteIndex :: IndexName a -> Result DeleteIndexResponse
deleteIndex ix = do
  c <- getter <$> ask
  m <- liftIO getGlobalManager
  let r = (mkWriteRequest' c)
          { path = [uri|/1/indexes/{ix}|]
          , method = methodDelete
          }
  liftIO $ withResponse r m $ \resp -> do
    aesonReader r resp

-- | Delete an index’s content, but leave settings and index-specific API keys untouched.
clearIndex :: IndexName a -> Result IndexOperationResponse
clearIndex ix = do
  c <- getter <$> ask
  m <- liftIO getGlobalManager
  let r = (mkReadRequest c)
          { path = [uri|/1/indexes/{ix}/clear|]
          , method = methodPost
          }
  liftIO $ withResponse r m $ \resp -> do
    aesonReader r resp

data AddObjectWithoutIdResponse a = AddObjectWithoutIdResponse
  { addObjectWithoutIdResponseCreatedAt :: UTCTime
  , addObjectWithoutIdResponseTaskId :: TaskId
  , addObjectWithoutIdResponseObjectId :: ObjectId a
  } deriving (Show, Eq)

instance FromJSON (AddObjectWithoutIdResponse a) where
  parseJSON = withObject "AddObjectWithoutIdResponse" $ \a -> AddObjectWithoutIdResponse
    <$> a .: "createdAt"
    <*> a .: "taskID"
    <*> a .: "objectID"


-- Note: ToJSON instance must produce a JSON object
-- | Add an object to the index, automatically assigning it an object ID.
addObjectWithoutId :: ToJSON a => IndexName a -> a -> Result (AddObjectWithoutIdResponse a)
addObjectWithoutId ix val = do
  c <- getter <$> ask
  m <- liftIO getGlobalManager
  let r = (mkWriteRequest c val)
          { path = [uri|/1/indexes/{ix}|]
          , method = methodPost
          }
  liftIO $ withResponse r m $ \resp -> do
    aesonReader r resp

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
-- | Add or replace an object with a given object ID. If the object does not exist, it will be created. If it already exists, it will be replaced.
--
-- Be careful: when an object already exists for the specified object ID, the whole object is replaced: existing attributes that are not replaced are deleted.
--
-- If you want to update only part of an object, use a partial update instead.

addObjectById :: ToJSON a => IndexName a -> ObjectId a -> a -> Result (AddObjectByIdResponse a)
addObjectById ix i val = do
  c <- getter <$> ask
  m <- liftIO getGlobalManager
  let r = (mkWriteRequest c val)
          { path = [uri|/1/indexes/{ix}/{i}|]
          , method = methodPut
          }
  liftIO $ withResponse r m $ \resp -> do
    aesonReader r resp

data UpdateOp
  = Increment Scientific
  | Decrement Scientific
  | Add (Either Scientific Text)
  | Remove (Either Scientific Text)
  | AddUnique (Either Scientific Text)

instance ToJSON UpdateOp where
  toJSON op = case op of
    Increment x -> mkOp "Increment" x
    Decrement x -> mkOp "Decrement" x
    Add x -> mkOp "Add" $ either toJSON toJSON x
    Remove x -> mkOp "Remove" $ either toJSON toJSON x
    AddUnique x -> mkOp "AddUnique" $ either toJSON toJSON x
    where
      mkOp :: forall a. ToJSON a => Text -> a -> Value
      mkOp t x = object [ "_operation" .= t, "value" .= x ]



data ObjectResponse = ObjectResponse

partiallyUpdateObject :: IndexName a -> HashMap Text UpdateOp -> Result ObjectResponse
partiallyUpdateObject = undefined

data RetrieveObjectResponse a = RetrieveObjectResponse
  { retrieveObjectResponseObjectId :: !(ObjectId a)
  , retrieveObjectResponseObject :: a
  } deriving (Show, Eq)

instance FromJSON a => FromJSON (RetrieveObjectResponse a) where
  parseJSON = withObject "RetrieveObjectResponse" $ \o -> do
    oid <- o .: "objectID"
    val <- parseJSON (Object $ H.delete "objectID" o) <|> parseJSON (Object o)
    return $ RetrieveObjectResponse oid val

retrieveObject
  :: FromJSON a
  => IndexName a
  -> ObjectId a
  -> [Text] -- ^ Attributes to retrieve. An empty list denotes retriving all attributes.
            -- Note that the FromJSON instance might not succeed if you exclude attributes, so
            -- it might be necessary to use @reconstrain@ on the index and id to use a different
            -- FromJSON instance
  -> Result (Maybe (RetrieveObjectResponse a))
retrieveObject ix oid attrs = do
  c <- getter <$> ask
  m <- liftIO getGlobalManager
  let r = (mkReadRequest c)
          { path = case attrs of
              [] -> [uri|/1/indexes/{ix}/{oid}|]
              attributesToRetrieve -> [uri|/1/indexes/{ix}/{oid}{?attributesToRetrieve}|]
          , method = methodGet
          }
  liftIO $ withResponse r m $ \resp -> do
    aesonReader r resp

newtype RetrieveObjectResults = RetrieveObjectResults { fromRetrieveObjectResults :: [Object] }
  deriving (Show, Eq)

instance FromJSON RetrieveObjectResults where
  parseJSON = withObject "RetrieveObjectResults" $ \rs -> do
    RetrieveObjectResults <$> rs .: "results"

retrieveObjects :: [(IndexName Object, ObjectId Object, [Text])] -> Result [Object]
retrieveObjects os = do
  c <- getter <$> ask
  m <- liftIO getGlobalManager
  let r = (mkReadRequest c)
          { path = [uri|/1/indexes/*/objects|]
          , method = methodPost
          , requestBody = RequestBodyLBS $ encode $ object
            [ "requests" .= map something os
            ]
          }
  fmap fromRetrieveObjectResults $ liftIO $ withResponse r m $ \resp -> do
    aesonReader r resp

  where
    something (ix, oid, ts) = object $ case ts of
      [] ->
        [ "indexName" .= ix
        , "objectID" .= oid
        ]
      as ->
        [ "indexName" .= ix
        , "objectID" .= oid
        , "attributesToRetrieve" .= ts
        ]

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
  c <- getter <$> ask
  m <- liftIO getGlobalManager
  let r = (mkWriteRequest' c)
          { path = [uri|/1/indexes/{ix}/{i}|]
          , method = methodDelete
          }
  liftIO $ withResponse r m $ \resp -> do
    aesonReader r resp

data BatchOp a
  = AddObjectOp a
  | UpdateObjectOp (ObjectId a) a
  | PartialUpdateObjectOp (ObjectId a) (HashMap Text UpdateOp)
  | PartialUpdateObjectNoCreateOp (ObjectId a) (HashMap Text UpdateOp)
  | DeleteObjectOp (ObjectId a)
  | DeleteIndexOp
  | ClearIndexOp

batchOpObject :: ToJSON a => Maybe (IndexName a) -> BatchOp a -> Value
batchOpObject mix op = tack $ case op of
  AddObjectOp x ->
    [ "action" .= ("addObject" :: Text)
    , "body" .= x
    ]
  UpdateObjectOp oid x ->
    [ "action" .= ("updateObject" :: Text)
    , "body" .= injectOid oid x
    ]
  PartialUpdateObjectOp oid x ->
    [ "action" .= ("partialUpdateObject" :: Text)
    , "body" .= injectOid (ObjectId $ fromObjectId oid) x
    ]
  PartialUpdateObjectNoCreateOp oid x ->
    [ "action" .= ("partialUpdateObjectNoCreate" :: Text)
    , "body" .= injectOid (ObjectId $ fromObjectId oid) x
    ]
  DeleteIndexOp ->
    [ "action" .= ("delete" :: Text)
    , "body" .= Object H.empty
    ]
  ClearIndexOp ->
    [ "action" .= ("clear" :: Text)
    , "body" .= Object H.empty
    ]
  where
    tack = object . maybe id ((:) . ("indexName" .=)) mix

injectOid :: ToJSON a => ObjectId a -> a -> Value
injectOid oid val = case toJSON val of
  Object o -> Object $ H.insert "objectID" (toJSON oid) o
  _ -> throw ToJsonInstanceMustProduceAnObject

data BatchResponse = BatchResponse
  { batchResponseTaskId :: TaskId
  , batchResponseObjectIds :: [Maybe (ObjectId Object)]
  } deriving (Show)

instance FromJSON BatchResponse where
  parseJSON = withObject "BatchResponse" $ \o -> BatchResponse
    <$> o .: "taskID"
    <*> o .: "objectIDs"

batch :: ToJSON a => IndexName a -> [BatchOp a] -> Result BatchResponse
batch ix ops = do -- TODO catch exceptions from injectOid
  c <- getter <$> ask
  m <- liftIO getGlobalManager
  let val = object
            [ "requests" .= map (batchOpObject Nothing) ops
            ]
  let r = (mkWriteRequest c val)
          { path = [uri|/1/indexes/{ix}/batch|]
          , method = methodPost
          }
  liftIO $ withResponse r m $ \resp -> do
    aesonReader r resp

data BatchMultipleIndicesResponse = BatchMultipleIndicesResponse
  { batchMultipleIndicesResponseTaskId :: HashMap (IndexName Object) TaskId
  , batchMultipleIndicesResponseObjectIds :: [Maybe (ObjectId Object)]
  } deriving (Show)

instance FromJSON BatchMultipleIndicesResponse where
  parseJSON = withObject "BatchMultipleIndicesResponse" $ \o -> BatchMultipleIndicesResponse
    <$> o .: "taskID"
    <*> o .: "objectIDs"

batchMultipleIndices :: [(IndexName Object, BatchOp Object)] -> Result BatchMultipleIndicesResponse
batchMultipleIndices ops = do
  c <- getter <$> ask
  m <- liftIO getGlobalManager
  let val = object
            [ "requests" .= map (\(k, v) -> batchOpObject (Just k) v) ops
            ]
  let r = (mkWriteRequest c val)
          { path = [uri|/1/indexes/*/batch|]
          , method = methodPost
          }
  liftIO $ withResponse r m $ \resp -> do
    aesonReader r resp

newtype Cursor a = Cursor { fromCursor :: Text }
  deriving (Show, Eq, ToJSON, FromJSON)

data BrowseIndexResponse a = BrowseIndexResponse
  { browseIndexResponseCursor :: Maybe (Cursor a)
  , browseIndexResponseHits :: !(Vector (RetrieveObjectResponse a))
  , browseIndexResponsePage :: !Int
  , browseIndexResponseNumberOfHits :: !Int
  , browseIndexResponseNumberOfPages :: !Int
  , browseIndexResponseHitsPerPage :: !Int
  , browseIndexResponseProcessingTimeMs :: !Int
  , browseIndexResponseQuery :: !Text
  , browseIndexResponseParams :: !Text
  }

instance (FromJSON a) => FromJSON (BrowseIndexResponse a) where
  parseJSON = withObject "BrowseIndexResponse" $ \o ->
    BrowseIndexResponse <$>
    o .:? "cursor" <*>
    o .: "hits" <*>
    o .: "page" <*>
    o .: "nbHits" <*>
    o .: "nbPages" <*>
    o .: "hitsPerPage" <*>
    o .: "processingTimeMS" <*>
    o .: "query" <*>
    o .: "params"

-- TODO support params
browseAllIndexContent :: FromJSON a => IndexName a -> Result (BrowseIndexResponse a)
browseAllIndexContent ix = do
  c <- getter <$> ask
  m <- liftIO getGlobalManager
  let r = (mkReadRequest c)
          { path = [uri|/1/indexes/{ix}/browse|]
          , method = methodGet
          }
  liftIO $ withResponse r m $ \resp -> do
    aesonReader r resp
{-
changeIndexSettings
-}

data IndexOperation = MoveIndex | CopyIndex
  deriving (Show, Eq)

data IndexOperationResponse = IndexOperationResponse
  { indexOperationResponseUpdatedAt :: UTCTime
  , indexOperationResponseTaskId :: TaskId
  } deriving (Show, Eq)

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
  c <- getter <$> ask
  m <- liftIO getGlobalManager
  let r =
        (mkWriteRequest c $
         object
           [ "operation" .=
             case op of
               MoveIndex -> "move" :: Text
               CopyIndex -> "copy"
           , "destination" .= to
           ])
        { path = [uri|/1/indexes/{from}/operation|] -- "/1/indexes/" <> fromIndexName from <> "/operation"
        , method = methodPost
        }
  liftIO $ withResponse r m $ \resp -> do aesonReader r resp

data TaskStatus = Published | NotPublished
  deriving (Show, Eq)

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

-- | Get the status of a task. This can be used to wait for a task to be processed via polling.
--
-- See 'waitTask' for an implementation of this behavior.
getTaskStatus :: IndexName a -> TaskId -> Result TaskStatusResult
getTaskStatus ix t = do
  c <- getter <$> ask
  m <- liftIO getGlobalManager
  let r = (mkReadRequest c)
          { path = [uri|/1/indexes/{ix}/task/{t}|]
          , method = methodGet
          }
  liftIO $ withResponse r m $ \resp -> do
    aesonReader r resp
{-
addIndexApiKey
updateIndexApiKey
listIndexApiKeys
listIndexApiKeysForAllIndices
retriveIndexApiKey
deleteIndexApiKey
-}

newtype FacetName = FacetName { fromFacetName :: Text }
  deriving (Show, Eq, FromJSON, FromJSONKey, Hashable)

instance ToTemplateValue FacetName where
  toTemplateValue = toTemplateValue . fromFacetName

newtype FacetQuery = FacetQuery { fromFacetQuery :: Text }
  deriving (Show, Eq)

data FacetHit = FacetHit
  { facetHitValue :: Text
  , facetHitHighlighted :: Text
  , facetHitCount :: Int
  } deriving (Show, Eq)

instance FromJSON FacetHit where
  parseJSON = withObject "FacetHit" $ \o -> FacetHit
    <$> o .: "value"
    <*> o .: "highlighted"
    <*> o .: "count"

newtype FacetHits = FacetHits
  { facetHits :: [FacetHit]
  } deriving (Show, Eq)

instance FromJSON FacetHits where
  parseJSON = withObject "FacetHits" $ \o -> FacetHits
    <$> o .: "facetHits"

searchFacetValues :: IndexName a -> FacetName -> FacetQuery -> Result FacetHits
searchFacetValues ix f q = do
  c <- getter <$> ask
  m <- liftIO getGlobalManager
  let r = (mkReadRequest c)
          { path = [uri|/1/indexes/{ix}/facets/{f}/query|]
          , method = methodPost
          }
  -- TODO add query to body
  -- object ["params" .= "facetQuery=?&params=?&maxFacetHits=10"]
  liftIO $ withResponse r m $ \resp -> do
    aesonReader r resp

newtype SynonymId = SynonymId { fromSynonymId :: Text }
  deriving (Show, Eq, ToJSON, FromJSON, FromJSONKey)

data Correction = Correction
  { correctionWord :: Text
  , correctionCorrections :: [Text]
  }

data Synonym
  = MultiWaySynonym [Text]
  | OneWaySynonym Text [Text]
  | AlternativeCorrection1 Correction
  | AlternativeCorrection2 Correction
  | Placeholder Text [Text]

{-
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
  = SynonymTy -- ^ Multi-way synonyms (a.k.a. “regular synonyms”). A set of words or phrases that are all substitutable to one another. Any query containing one of them can match records containing any of them.
  | OneWaySynonymTy -- ^ One-way synonym. Alternative matches for a given input. If the input appears inside a query, it will match records containing any of the defined synonyms. The opposite is not true: if a synonym appears in a query, it will not match records containing the input, nor the other synonyms.
  | AltCorrection1Ty -- ^ Alternative corrections. Same as a one-way synonym, except that when matched, they will count as 1 (respectively 2) typos in the ranking formula.
  | AltCorrection2Ty
  | PlaceholderTy -- ^ A placeholder is a special text token that is placed inside records and can match many inputs. For more information on synonyms, please read our Synonyms guide. https://www.algolia.com/doc/guides/textual-relevance/synonyms/

synonymTypeName :: SynonymType -> Text
synonymTypeName SynonymTy = "synonym"
synonymTypeName OneWaySynonymTy = "onewaysynonym"
synonymTypeName AltCorrection1Ty = "altcorrection1"
synonymTypeName AltCorrection2Ty = "altcorrection2"
synonymTypeName PlaceholderTy = "placeholder"

data SynonymSearch = SynonymSearch
  { synonymSearchQuery :: Maybe Text -- ^ Search for specific synonyms matching this string.
  , synonymSearchType :: [SynonymType] -- ^ Only search for specific types of synonyms.
  , synonymSearchPage :: Maybe Int -- ^ Number of the page to retrieve (zero-based).
  , synonymSearchHitsPerPage :: Maybe Int -- ^ Maximum number of synonym objects to retrieve.
  }

instance ToJSON SynonymSearch where
  toJSON SynonymSearch{..} = object
    [ "query" .= synonymSearchQuery
    , "type" .= case synonymSearchType of
        [] -> Null
        ts -> Data.Aeson.String $ T.intercalate "," $ map synonymTypeName ts
    , "page" .= synonymSearchPage
    , "hitsPerPage" .= synonymSearchHitsPerPage
    ]

data SynonymSearchResponse = SynonymSearchResponse Object

instance FromJSON SynonymSearchResponse where
  parseJSON = withObject "SynonymSearchResponse" (return . SynonymSearchResponse)

-- | Search or browse all synonyms, optionally filtering them by type.
searchSynonyms :: IndexName a -> SynonymSearch -> Result SynonymSearchResponse
searchSynonyms ix params = do
  c <- getter <$> ask
  m <- liftIO getGlobalManager
  let r = (mkReadRequest c)
          { path = "/1/indexes/" <> fromIndexName ix <> "/synonyms/search"
          , method = methodPost
          , requestBody = RequestBodyLBS $ encode params
          }
  liftIO $ withResponse r m $ \resp -> do
    aesonReader r resp

{-
addApiKey
updateApiKey
listApiKeys
getApiKey
deleteApiKey
-}

data LogType
  = AllLogs
  | QueryLogs
  | BuildLogs
  | ErrorLogs
  deriving (Show)

data LogsResponse = LogsResponse
  { logsResponseResults :: [Object]
  } deriving (Show)

instance FromJSON LogsResponse where
  parseJSON = withObject "LogsResponse" $ \r ->
    LogsResponse <$> r .: "logs"

getLogs ::
     Maybe Int -- ^ Offset
  -> Maybe Int -- ^ Length
  -> Maybe (IndexName a)
  -> Maybe LogType
  -> Result LogsResponse
getLogs o l ix t = do
  c <- getter <$> ask
  m <- liftIO getGlobalManager
  let r = (mkReadRequest c)
          { path = [uri|/1/logs{?params*}|]
          , method = methodGet
          }
  liftIO $ withResponse r m $ \resp -> do
    aesonReader r resp
  where
    params = AList (("offset" :: Text, fmap (T.pack . show) o) : ("length", fmap (T.pack . show) l) : ("type", fmap renderTy t) : ("indexName", (decodeUtf8 . fromIndexName) <$> ix) : [])
    renderTy ty = case ty of
      AllLogs -> "all"
      QueryLogs -> "query"
      BuildLogs -> "build"
      ErrorLogs -> "error"

{- |
You may have a single index containing per-user data. In that case, you may wish to restrict access to the records of one particular user. Typically, all your records would be tagged with their associated user_id, and you would add a filter at query time like filters=user_id:${requested_id} to retrieve only what the querying user has access to.

Adding that filter directly from the frontend (browser or mobile application) will result in a security breach, because the user would be able to modify the filters you’ve set, e.g. by modifying the JavaScript code.

In order to keep sending the query from the browser (which is recommended for optimal latency) but only target secured records, you can generate secured API keys from your backend and use them in your frontend code. The backend will then automatically enforce the security filters contained in the key; the user will not be able to alter them.

A secured API key is used like any other API key via the X-Algolia-API-Key request header.

Generate a secured API key
Secured API keys are generated by hashing (HMAC SHA-256) the following criteria together:

a private API key (can be any API Key that is not the admin API Key), used as the secret for HMAC SHA-256;

a URL-encoded list of query parameters defining the security filters.

The result of the hashing is concatenated to the URL-encoded query parameters, and this content is encoded in Base64 to generate the final secured API key.
-}
generateSecuredApiKey :: ApiKey -> Query -> Maybe ByteString -> ByteString
generateSecuredApiKey privateKey qps' userKey = convertToBase Base64 (hmac <> qps)
  where
    ctxt :: Context SHA256
    ctxt = initialize $ fromApiKey privateKey
    hmac = convertToBase Base16 $ hmacGetDigest $ finalize $ update ctxt qps
    qps = case userKey of
      Nothing -> renderQuery False qps'
      Just uk -> renderQuery False (qps' <> [("userToken", Just uk)])

-- | Wait for a task to be processed by Algolia
waitTask :: IndexName a -> TaskId -> Result ()
waitTask ix taskId =
  void $
  retrying
    (capDelay 5000000 (exponentialBackoff 100000))
    statusChecker
    statusGetter
  where
    statusGetter _ = getTaskStatus ix taskId
    statusChecker _ status =
      return $
      case taskStatusResultStatus status of
        NotPublished -> True
        Published -> False
