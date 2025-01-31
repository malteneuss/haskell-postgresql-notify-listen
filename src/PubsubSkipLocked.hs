{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, asks, liftIO)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.UUID (UUID)
import Pubsub.PubsubModels (Key (..), Task (..))
import Control.Concurrent (forkIO, threadDelay, newEmptyMVar, takeMVar, putMVar, MVar)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import GHC.Generics ( Generic )
import System.Environment (getEnv)
import Control.Monad.Logger (runStdoutLoggingT)
import qualified Data.ByteString.Char8  as BS8
import qualified Data.ByteString  as BS
import Database.Persist.Postgresql (createPostgresqlPool, printMigration, runSqlPool)
import Database.PostgreSQL.Simple.Notification (getNotification, Notification(..))
import Database.PostgreSQL.Simple (connectPostgreSQL, Connection, execute_, query_, execute, FromRow, Only(..), ToRow)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Only(..))


data MyTask = MyTask {
    id :: Int,
    payload :: Text,
    createdAt :: UTCTime
}
    deriving stock (Show, Generic)
    deriving anyclass (FromRow)
    
main :: IO ()
main = do
    -- we have to use postgresql-simple to listen for notifications
    -- https://hackage.haskell.org/package/postgresql-simple/docs/Database-PostgreSQL-Simple-Notification.html#v:getNotification
    connectionStr <- getEnv "DATABASE_URL"
    forkIO $ do
        -- receive notifications on a separate thread, https://www.postgresql.org/docs/current/sql-listen.html
        conn <- connectPostgreSQL (BS8.pack connectionStr)
        void $ execute_ conn "LISTEN task_listener"
        forever $ do
            tasks <- query_ @MyTask conn "BEGIN; SELECT * FROM task FOR UPDATE SKIP LOCKED LIMIT 1;"
            if null tasks
                then do 
                    putStrLn "No tasks available. Wait for new entries via NOTIFY."
                    void $ execute_ conn "COMMIT;"
                    notification <- getNotification conn
                    putStrLn $ "Received notification: " ++ (show $ notification)
                else do
                    let task = head tasks
                    putStrLn $ "Received task: " ++ (show $ task)
                    void $ execute conn "DELETE FROM task WHERE id = ?" (Only (task.id))
                    void $ execute_ conn "COMMIT;"
            -- putStrLn $ "Received notification: " ++ (show $ notificationData notification)
    conn <- connectPostgreSQL (BS8.pack connectionStr)
    -- void $ execute @(String) conn "INSERT INTO tasks (payload) VALUES (?)" ("new task payload")
    void $ execute_ conn "INSERT INTO task (payload) VALUES ('a payload')"
        -- trigger a notification, https://www.postgresql.org/docs/current/sql-notify.html
    -- Block the main thread
    threadDelay 10000000  -- wait for 10 seconds
