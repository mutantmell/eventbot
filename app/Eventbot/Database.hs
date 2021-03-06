{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Eventbot.Database where

import qualified Data.Text as T
import           Database.SQLite.Simple (NamedParam(..))
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromRow as SQL

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Int
import           Data.Maybe
import           GHC.Generics

data EventDatabase = EventDatabase
  { serverQuery :: (forall m . MonadIO m => Int64 -> m (Maybe ServerRow))
  , insertServerQuery :: (forall m . MonadIO m => Int64 -> Int64 -> m Int64)
  , messagesQuery :: (forall m . MonadIO m => Int64 -> m [MessageRow])
  , insertMessageQuery :: (forall m . MonadIO m => Int64 -> Int64 -> T.Text -> m Int64)
  , deleteMessageQuery :: (forall m . MonadIO m => Int64 -> m ())
  }

data ServerRow = ServerRow
  { serverRowId :: Int64
  , serverId :: Int64
  , channelId :: Int64
  } deriving (Show, Eq, Generic)

instance SQL.FromRow ServerRow where
  fromRow = ServerRow <$> SQL.field <*> SQL.field <*> SQL.field

instance SQL.ToRow ServerRow where
  toRow (ServerRow id_ svr chnl) = SQL.toRow (id_, svr, chnl)

data MessageRow = MessageRow
  { messageRowId :: Int64
  , messageServerId :: Int64
  , messageId :: Int64
  , message :: T.Text
  } deriving (Show, Eq, Generic)

instance SQL.FromRow MessageRow where
  fromRow = MessageRow <$> SQL.field <*> SQL.field <*> SQL.field <*> SQL.field

instance SQL.ToRow MessageRow where
  toRow (MessageRow id_ svr msgid msg) = SQL.toRow (id_, svr, msgid, msg)

rowForServer :: String -> Int64 -> IO (Maybe ServerRow)
rowForServer db serverId = listToMaybe <$> dbResult
  where
    dbResult = SQL.withConnection db $ \conn ->
      SQL.query conn "SELECT * FROM servers WHERE serverId = ?" (SQL.Only serverId)

insertServer :: String -> Int64 -> Int64 -> IO Int64
insertServer db serverId channelId = SQL.withConnection db $ \conn -> do
  SQL.execute conn "INSERT INTO servers (serverId, channelId) VALUES (?, ?)" (serverId, channelId)
  SQL.lastInsertRowId conn

messagesForServer :: String -> Int64 -> IO [MessageRow]
messagesForServer db serverId = SQL.withConnection db $ \conn ->
  SQL.query conn "SELECT * FROM messages WHERE serverId = ?" (SQL.Only serverId)

insertMessage :: String -> Int64 -> Int64 -> T.Text -> IO Int64
insertMessage db serverId messageId message = SQL.withConnection db $ \conn -> do
  SQL.execute conn "INSERT INTO messages (serverId, messageId, message) VALUES (?, ?, ?)" (serverId, messageId, message)
  SQL.lastInsertRowId conn

deleteMessage :: String -> Int64 -> IO ()
deleteMessage db messageId = SQL.withConnection db $ \conn ->
  SQL.execute conn "DELETE FROM messages WHERE id = ?" (SQL.Only messageId)

mkServer :: String -> IO ()
mkServer db = SQL.withConnection db $ \conn -> do
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS servers (id INTEGER PRIMARY KEY, serverId Integer, channelId Integer)"
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS messages (id INTEGER PRIMARY KEY, serverId Integer, messageId Integer, message TEXT)"

mkDatabaseFor :: String -> EventDatabase
mkDatabaseFor db = EventDatabase 
  { serverQuery = \s -> liftIO $ rowForServer db s
  , insertServerQuery = \s c -> liftIO $ insertServer db s c
  , messagesQuery = \s -> liftIO $ messagesForServer db s
  , insertMessageQuery = \s m msg -> liftIO $ insertMessage db s m msg
  , deleteMessageQuery = \m -> liftIO $ deleteMessage db m
  }
