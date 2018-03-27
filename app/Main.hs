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
import qualified Pipes as P
import qualified Network.Discord as D

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Pretty

import qualified Network.Google as Google
import qualified Network.Google.Auth as Google
import qualified Network.Google.AppsCalendar as Calendar
import qualified Network.Google.Auth.ServiceAccount as ServiceAccount

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Char as C
import qualified Data.Foldable as F

import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromRow as SQL

import Network.HTTP.Conduit (Manager, newManager, tlsManagerSettings)
import qualified Network.HTTP.Conduit as Conduit

import qualified Options.Applicative as Opt

import Data.String.Conversions

import Control.Monad.IO.Class

import Control.Lens

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Data.Either
import Data.Functor
import Data.Monoid
import GHC.Generics
import System.IO (stdout)

import Data.Time

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

data Command = EventCommand EventSubCommand
  deriving (Eq, Show, Generic)

data EventSubCommand = GetEvents 
                     | CreateEvent
  deriving (Eq, Show, Generic)

command :: Text -> Maybe Command
command str = case arguments of
  ("!event":rest) -> EventCommand <$> eventSubCommand rest
  _               -> Nothing
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

data GoogleEnv = GoogleEnv
  { manager :: Conduit.Manager
  , logger :: Google.Logger
  , credentials :: (forall s . Google.Credentials s)
  }

mkGoogleEnv :: Config.Config -> IO GoogleEnv
mkGoogleEnv config = do
  clientId <- Google.ClientId <$> Config.require config "client-id"
  clientSecret <- Google.Secret <$> Config.require config "client-secret"
  refreshToken <- Google.RefreshToken <$> Config.require config "refresh-token"
  let oauthClient = Google.OAuthClient clientId clientSecret
      credentials = Google.FromUser $ Google.AuthorizedUser clientId refreshToken clientSecret
  lgr <- Google.newLogger Google.Debug stdout
  mgr <- newManager tlsManagerSettings
  pure $ GoogleEnv mgr lgr credentials

data CalendarData = CalendarData
  { calendarId :: Text
  } deriving (Eq, Show, Generic)

data GoogleStateException = NoEventCalendarException deriving (Show, Eq)

instance Exception GoogleStateException

runCalendar :: GoogleEnv -> Google.Google '["https://www.googleapis.com/auth/calendar"] b -> IO b
runCalendar GoogleEnv{..} task = do
  env <- Google.newEnvWith credentials logger manager <&> (Google.envScopes .~ Calendar.calendarScope)
  Google.runResourceT . Google.runGoogle env $ task

getCalendar :: Text -> Google.Google '["https://www.googleapis.com/auth/calendar"] CalendarData
getCalendar name = do
  out <- Google.send Calendar.calendarListList
  let calendars = out ^.. Calendar.clItems . traverse
      maybeCalendar = F.find (\v -> v ^. Calendar.cleSummary == Just name) calendars
      maybeId = maybeCalendar >>= view Calendar.cleId
  maybe (throwM NoEventCalendarException) (pure . CalendarData) maybeId

insertTestEvent :: CalendarData -> Google.Google '["https://www.googleapis.com/auth/calendar"] Calendar.Event
insertTestEvent calendar = do
  --Google.send $ Calendar.calendarsGet (calendarId calendar)
  timeZone <- liftIO $ zonedTimeZone <$> getZonedTime
  let day = fromGregorian 2018 3 28
      startTimeOfDay = TimeOfDay 17 0 0
      startLocalTime = LocalTime day startTimeOfDay
      startUtcTime = localTimeToUTC timeZone startLocalTime
      endTimeOfDay = TimeOfDay 19 0 0
      endLocalTime = LocalTime day endTimeOfDay
      endUtcTime = localTimeToUTC timeZone endLocalTime
      eventStart = Calendar.eventDateTime & Calendar.edtDateTime .~ Just startUtcTime
                                          & Calendar.edtTimeZone .~ Just "America/Los_Angeles"
      eventEnd = Calendar.eventDateTime & Calendar.edtDateTime .~ Just endUtcTime
                                          & Calendar.edtTimeZone .~ Just "America/Los_Angeles"
      event = Calendar.event & Calendar.eSummary .~ Just "test event"
                             & Calendar.eStart .~ Just eventStart
                             & Calendar.eEnd .~ Just eventEnd
  Google.send $ Calendar.eventsInsert (calendarId calendar) event

main :: IO ()
main = do
  Args{..} <- Opt.execParser arguments
  config <- Config.loadGroups [
    ("discord.", Config.Required $ "." </> "conf" </> "discord.conf"),
    ("google.", Config.Required $ "." </> "conf" </> "google.conf")
    ]
  --botToken <- Config.require config "discord.bot-token"
  --D.runBot (D.Bot botToken) discord
  env <- mkGoogleEnv (Config.subconfig "google" config)

  calendarName <- Config.require config "google.calendar-name"

  when argsInit $ runCalendar env $ do
    let newCalendar = Calendar.calendar & Calendar.calSummary .~ Just calendarName
        request = Calendar.calendarsInsert newCalendar
    Google.send request $> ()
  calendar <- runCalendar env $ getCalendar calendarName
  out <- runCalendar env $ insertTestEvent calendar
  print out