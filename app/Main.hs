{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
import qualified Network.Discord as D

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Pretty

import qualified Network.Google as Google
import qualified Network.Google.Auth as Google
import qualified Network.Google.AppsCalendar as Calendar
import qualified Network.Google.Auth.ServiceAccount as ServiceAccount

import qualified Data.Char as C
import qualified Data.Foldable as F
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TL
import           Data.Time (LocalTime(..), TimeOfDay(..), TimeZone)
import qualified Data.Time as Time

import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromRow as SQL

import Network.HTTP.Conduit (Manager)
import qualified Network.HTTP.Conduit as Conduit

import qualified Options.Applicative as Opt

import Data.String.Conversions

import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Either
import Data.Functor
import Data.Monoid
import GHC.Generics
import System.IO (stdout)

import Eventbot.Google.Calendar
import Eventbot.Commands
import Eventbot.Commands.Optics
import Eventbot.Discord

data Args = Args
  { argsInit :: Bool
  } deriving (Eq, Show)

arguments :: Opt.ParserInfo Args
arguments = Opt.info (parser <**> Opt.helper) description
  where
    description = Opt.fullDesc
               <> Opt.progDesc "Run a Discord bot to help plan and organize calendar events"
               <> Opt.header "calendar-bot"
    parser = Args
          <$> Opt.switch 
           (  Opt.long "init"
           <> Opt.help "Whether to initialize the bot for a first run"
           )

mkGoogleEnv :: Config.Config -> IO GoogleEnv
mkGoogleEnv config = do
  clientId <- Google.ClientId <$> Config.require config "client-id"
  clientSecret <- Google.Secret <$> Config.require config "client-secret"
  refreshToken <- Google.RefreshToken <$> Config.require config "refresh-token"
  let oauthClient = Google.OAuthClient clientId clientSecret
      credentials = Google.FromUser $ Google.AuthorizedUser clientId refreshToken clientSecret
  lgr <- Google.newLogger Google.Error stdout
  mgr <- Conduit.newManager Conduit.tlsManagerSettings
  pure $ GoogleEnv mgr lgr credentials

main :: IO ()
main = do
  Args{..} <- Opt.execParser arguments
  config <- Config.loadGroups [
    ("discord.", Config.Required $ "." </> "conf" </> "discord.conf"),
    ("google.", Config.Required $ "." </> "conf" </> "google.conf")
    ]

  timeZone <- Time.zonedTimeZone <$> Time.getZonedTime

  env <- mkGoogleEnv (Config.subconfig "google" config)
  calendarName <- Config.require config "google.calendar-name"

  when argsInit $ runCalendar env $ do
    let newCalendar = Calendar.calendar & Calendar.calSummary .~ Just calendarName
        request = Calendar.calendarsInsert newCalendar
    Google.send request $> ()

  maybeCalendar <- runCalendar env $ getCalendar calendarName
  calendar <- maybe (throwM NoEventCalendarException) pure maybeCalendar

  botToken <- Config.require config "discord.bot-token"
  D.runBot (D.Bot botToken) $ discord timeZone env calendar

  pure ()
