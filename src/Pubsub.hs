{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, asks, liftIO)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.UUID (UUID)
import Database.Persist.Sql (ConnectionPool, Entity (..), PersistEntity (..), SelectOpt (Asc, LimitTo, OffsetBy), SqlBackend, fromSqlKey, get, getBy, insert, insertKey, runSqlPool, selectList, (!=.), (=.), (==.), Single (..))
import Pubsub.PubsubModels (Key (..), Task (..))
import Database.Persist.Postgresql (rawExecute, rawSql)
import Control.Concurrent (forkIO, threadDelay, newEmptyMVar, takeMVar, putMVar, MVar)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.Persist.Sql (SqlBackend, rawExecute, rawSql)
import GHC.Generics ( Generic )
import System.Environment (getEnv)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString qualified as BS
import Database.Persist.Postgresql (createPostgresqlPool, printMigration, runSqlPool)
import Database.PostgreSQL.Simple.Notification (getNotification, Notification(..))
import Database.PostgreSQL.Simple (connectPostgreSQL, Connection, execute_)

main :: IO ()
main = do
    -- we have to use postgresql-simple to listen for notifications
    -- https://hackage.haskell.org/package/postgresql-simple/docs/Database-PostgreSQL-Simple-Notification.html#v:getNotification
    -- but can use Persistent for everything else
    connectionStr <- getEnv "DATABASE_URL"
    -- mvar <- newEmptyMVar
    forkIO $ do
        -- receive notifications on a separate thread, https://www.postgresql.org/docs/current/sql-listen.html
        conn <- connectPostgreSQL (BS8.pack connectionStr)
        void $ execute_ conn "LISTEN task_listener"
        forever $ do
            notification <- getNotification conn
            -- putStrLn $ "Received notification: " ++ (show $ notificationData notification)
            putStrLn $ "Received notification: " ++ (show $ notification)
            threadDelay 1000000  -- wait for 1 second before checking for the next notification 
            -- putMVar mvar ()
    pool <- runStdoutLoggingT $ createPostgresqlPool (BS8.pack connectionStr) 1

    flip runSqlPool pool $ do
        -- trigger a notification, https://www.postgresql.org/docs/current/sql-notify.html
        void $ insert $ Task "payload"
        void $ rawExecute "NOTIFY task_listener, 'test message'" []
        -- rawSql doesn't seem to work, because that function always expects a return value from Postgres that'll never come.
        -- void $ rawSql @(Single (Maybe BS.ByteString)) "SELECT pg_notify('task_listener', 'test')" []
    -- Block the main thread
    -- takeMVar mvar
    threadDelay 10000000  -- wait for 10 seconds before checking for the next notification 
