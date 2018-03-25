{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import           System.FilePath ((</>))
import qualified System.FilePath as Path
import qualified Network.WebSockets as WS
import qualified Wuss as Wuss
import qualified Data.Configurator as Config
import qualified Data.Configurator.Types as Config
import qualified Network.OAuth.OAuth2 as OAuth
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified Data.ByteString as BS
import qualified URI.ByteString as Uri
import qualified Pipes as P
import qualified Network.Discord as D

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Char as C

import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromRow as SQL

import Data.String.Conversions

import Control.Lens.Getter

import Control.Applicative
import Control.Exception
import Data.Either
import Data.Functor
import GHC.Generics

data UriParseException = UriParseException BS.ByteString
  deriving (Show)
instance Exception UriParseException

-- data OAuthConfig = OAuthConfig
--   { clientId :: !Text
--   , clientSecret :: !Text
--   , authorizeUri :: !(Uri.URIRef Uri.Absolute)
--   , tokenUri :: !(Uri.URIRef Uri.Absolute)
--   } deriving (Eq, Show, Generic)

mkURI :: BS.ByteString -> IO (Uri.URIRef Uri.Absolute)
mkURI str = either (const $ throwIO err) pure parse
  where
    parse = Uri.parseURI Uri.strictURIParserOptions str
    err = UriParseException str

mkOAuth :: Config.Config -> IO OAuth.OAuth2
mkOAuth config = OAuth.OAuth2 
    <$> Config.require config "client-id"
    <*> Config.require config "client-secret"
    <*> (mkURI =<< Config.require config "authorize-uri")
    <*> (mkURI =<< Config.require config "token-uri")
    <*> pure Nothing

data DiscordConf = DiscordConf 
  { oauth :: !OAuth.OAuth2
  } deriving (Eq)

data Command = EventCommand EventSubCommand
  deriving (Eq, Show)

data EventSubCommand = GetEvents 
                     | CreateEvent
  deriving (Eq, Show)

command :: Text -> Maybe Command
command str = case arguments of
  ("!event":rest) -> EventCommand <$> eventSubCommand rest
  where
    arguments = T.split C.isSpace str
    eventSubCommand :: [Text] -> Maybe EventSubCommand
    eventSubCommand ("all":[])    = Just GetEvents
    eventSubCommand ("create":_) = Just CreateEvent
    eventSubCommand _            = Nothing


reply :: D.Message -> Text -> P.Effect D.DiscordM ()
reply D.Message{D.messageChannel=chan} cont = D.fetch' $ D.CreateMessage chan cont Nothing

discord :: D.DiscordBot D.BotClient ()
discord = do
  D.with D.ReadyEvent $ \(D.Init v u _ _ _) ->
    D.liftIO $ putStrLn $ "Connected to gateway v" ++ show v ++ " as user " ++ show u

  D.with D.MessageCreateEvent $ \msg@D.Message{..} -> do 
    D.when (not $ D.userIsBot messageAuthor) $ do
      D.when ("Ping" `T.isPrefixOf` messageContent) $
        reply msg "Pong!"
      D.when ("!" `T.isPrefixOf` messageContent) $
        mapM_ (reply msg) ((convertString . show) <$> command messageContent)

main :: IO ()
main = do
  config <- Config.load [ Config.Required $ "." </> "conf" </> "discord.conf" ]
  botToken <- Config.require config "bot-token"
  D.runBot (D.Bot botToken) discord
