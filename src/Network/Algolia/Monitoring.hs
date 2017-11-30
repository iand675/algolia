{-# LANGUAGE OverloadedStrings #-}
module Network.Algolia.Monitoring where

import Data.Aeson
import Data.Scientific
import Data.Text (Text)
import Data.Vector (Vector)
-- getCurrentApiStatus
-- getCurrentStatusByServer
-- listLastIncidents
-- listLastIncidentsByServer

data Statistic
  = SearchOperations
  | AclOperations
  | IndexingOperations
  | RecordOperations
  | SynonymOperations
  | TotalIndexingOperations
  | TotalSearchOperations
  | TotalAclOperations
  | TotalRecordsOperations
  | TotalOperations
  | TotalSynonymOperations
  | Records
  | MaxQps
  | AverageProcessingTime
  | DataSize
  | FileSize
  | Everything

data StatisticPeriod
  = DayStatistics
  | MonthStatistics
  | YearStatistics

data TimeSeriesData = TimeSeriesData
  { timeSeriesTimestamp :: !Int
  , timeSeriesValue :: !Int
  } deriving (Show, Eq)

instance FromJSON TimeSeriesData where
  parseJSON = withObject "TimeSeriesData" $ \o ->
    TimeSeriesData <$>
    o .: "t" <*>
    o .: "v"

newtype TimeSeriesRecords = TimeSeriesRecords
  { timeSeriesRecords :: Vector TimeSeriesData
  } deriving (Show, Eq)

instance FromJSON TimeSeriesRecords where
  parseJSON = withObject "TimeSeriesRecords" $ \o ->
    TimeSeriesRecords <$>
    o .: "records"

-- getUsage
-- getUsageIndex

data InventoryServer = InventoryServer
  { inventoryServerName :: Text
  , inventoryServerRegion :: Text
  , inventoryServerIsReplica :: Bool
  , inventoryServerCluster :: Text
  } deriving (Show, Eq)

instance FromJSON InventoryServer where
  parseJSON = withObject "InventoryServer" $ \o ->
    InventoryServer <$>
    o .: "name" <*>
    o .: "region" <*>
    o .: "is_replica" <*>
    o .: "cluster"

-- getInventoryServers
-- o .: "inventory"

data InventoryProbe = InventoryProbe
  { inventoryProbeName :: Text
  , inventoryProbeCity :: Text
  , inventoryProbeCountry :: Text
  , inventoryProbeLatitude :: Scientific
  , inventoryProbeLongitude :: Scientific
  , inventoryProbeRegions :: [Text]
  } deriving (Show, Eq)

instance FromJSON InventoryProbe where
  parseJSON = withObject "InventoryProbe" $ \o ->
    InventoryProbe <$>
    o .: "name" <*>
    o .: "city" <*>
    o .: "country" <*>
    o .: "latitude" <*>
    o .: "longitude" <*>
    o .: "regions"

-- getInventoryProbes
-- o .: "probes"

-- getAverageLatency
-- getRelevantLatency
-- getOthersLatency
-- getIndexingTime
-- getServerReachability

-- | This is only available for enterprise customers
-- getInfrastructureMetrics

