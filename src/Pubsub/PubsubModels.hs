{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use toString" #-}
{-# HLINT ignore "Use 'readMaybe' from Relude" #-}
{-# HLINT ignore "Redundant pack" #-}
{-# HLINT ignore "Use toText" #-}

module Pubsub.PubsubModels where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.Persist.Class (PersistField)
import Database.Persist.Sql (ConnectionPool, Entity (..), PersistEntity (..), PersistFieldSql (..), SelectOpt (Asc, LimitTo, OffsetBy), SqlBackend, SqlType (..), fromPersistValue, fromSqlKey, get, getBy, insert, insertKey, runSqlPool, selectList, toPersistValue, (!=.), (=.), (==.))
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Database.Persist.Types (PersistValue (..))
import GHC.Generics (Generic)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
        Task
            -- implicit id serial primary key
            payload Text
            createdAt UTCTime default=now() MigrationOnly
            deriving Show
            deriving Generic
            deriving FromJSON
        |]
