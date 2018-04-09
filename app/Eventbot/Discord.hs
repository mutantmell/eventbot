{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eventbot.Discord
( discord

) where

import qualified Data.Aeson as Json
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (Day, TimeZone, UTCTime)
import qualified Data.Time as Time

import qualified Database.SQLite.Simple as SQL
import qualified Network.Discord as D
import qualified Network.Discord.Rest as DR
import qualified Network.Discord.Rest.Guild as DR
import qualified Pipes as P

import qualified Data.Foldable as F

import Data.String.Conversions

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Int
import Data.Functor
import Data.Maybe
import Data.Monoid
import GHC.Exts (fromList)
import Unsafe.Coerce
import Formatting
import Formatting.Time

import Eventbot.Commands
import Eventbot.Commands.Optics
import Eventbot.Database
import Eventbot.Google.Calendar

{-
-- bad code that gets the full guild def
    (D.SyncFetched out) <- D.fetch $ DR.GetGuild (D.guildId $ head guild)
    let gotten = (unsafeCoerce out :: D.Guild)
    case gotten of 
      D.Unavailable _ -> do
        liftIO $ putStrLn $ "Error: could not fetch guild data"
      _ -> do
        liftIO $ print gotten
-}

maybeTextChannelName :: D.Channel -> Maybe (D.Snowflake, Text)
maybeTextChannelName D.Text{..} = Just (channelId, (convertString channelName))
maybeTextChannelName other = Nothing

snowflakeToInt :: D.Snowflake -> Int64
snowflakeToInt (D.Snowflake int) = fromIntegral int

discord :: TimeZone -> GoogleEnv -> CalendarData -> EventDatabase -> D.DiscordBot D.BotClient ()
discord timeZone googleEnv calendar database = do
  D.with D.ReadyEvent $ \msg@(D.Init v u _ guilds _) -> do
    F.for_ guilds $ \guild -> do
      let gid = D.guildId guild
      (liftIO $ (serverQuery database) (snowflakeToInt gid)) >>= \case
        Just _ -> pure ()
        Nothing -> do
          (D.SyncFetched !channelsRaw) <- D.fetch $ DR.GetGuildChannels gid
          let !channels = (unsafeCoerce channelsRaw :: [D.Channel])
          let maybeChannel = F.find (\(_, name) -> name == "events") (mapMaybe maybeTextChannelName channels)
          channelId <- case maybeChannel of
            Just (channelId, _) -> pure channelId
            Nothing -> do
              let newChan = Json.Object $ fromList [ ("name", Json.String "events") ]
              (D.SyncFetched !newChannelRaw) <- D.fetch $ DR.CreateGuildChannel gid newChan
              let !newChannel = (unsafeCoerce newChannelRaw :: D.Channel)
              pure $ D.channelId newChannel
          liftIO $ (insertServerQuery database) (snowflakeToInt gid) (snowflakeToInt channelId)
          pure ()

  D.with D.MessageCreateEvent $ \msg@D.Message{..} -> do
    D.when (not $ D.userIsBot messageAuthor) $ do
      D.when ("!" `T.isPrefixOf` messageContent) $ do
        let maybeMsg = parseCommand messageContent
        forM_ maybeMsg $ \case
          (EventCommand (GetEvents getData)) -> do
            fromDay <- maybe (startOfToday timeZone) (pure . startOfDay timeZone) (maybeFromDay getData)
            let getRequest = getEventsFromDateGoogle fromDay calendar
            (liftIO $ runCalendar googleEnv getRequest) >>= \case
              [] -> reply msg "no upcoming events" $> ()
              events -> do
                let names = formatEvent timeZone <$> events
                mapM_ (reply msg) names
          (EventCommand GetCalendar) -> do
            reply msg "getting calendar link..."
            pure ()
          (EventCommand (CreateEvent eventData)) -> do
            let calendarRequest = eventData ^. createEventIso timeZone
                insertRequest = calendarRequestToGoogle calendar calendarRequest
            liftIO $ runCalendar googleEnv insertRequest
            reply msg $ "created event " <> Eventbot.Commands.eventName eventData
            pure ()
          (EventCommand InvalidEventCommand) -> do
            reply msg "Invalid command.  Help..."
            pure ()

reply :: D.Message -> Text -> P.Effect D.DiscordM ()
reply D.Message{D.messageChannel=chan} cont = D.fetch' $ D.CreateMessage chan cont Nothing

startOfToday :: (MonadIO io) => TimeZone -> io UTCTime
startOfToday timeZone = liftIO $ do
  zonedTime <- Time.getZonedTime
  let today = Time.localDay (Time.zonedTimeToLocalTime zonedTime)
      startOfToday = Time.LocalTime today Time.midnight
  pure $ Time.localTimeToUTC timeZone startOfToday

startOfDay :: TimeZone -> Day -> UTCTime
startOfDay timeZone day = Time.localTimeToUTC timeZone startOfDay'
  where
    startOfDay' = Time.LocalTime day Time.midnight

formatEvent :: TimeZone -> CalendarEvent -> Text
formatEvent tz CalendarEvent{..} = template eventName maybeLocation
  where
    time start end = if Time.localDay start == Time.localDay end
      then sformat ("from " % dateSlash % " " % hm % " " % dayHalfU % " until " % hm % " " % dayHalfU) start start start end end
      else sformat ("from " % dateSlash % " " % hm % " " % dayHalfU % " until " % dateSlash % " " % hm % " " % dayHalfU) start start start end end end
    time _ _ = "foo"
    timeStr = time (Time.utcToLocalTime tz startTime) (Time.utcToLocalTime tz endTime)
    noLocTemplate = "Event: " % stext % "\ndate: " % stext
    withLocTemplate = "Event: " % stext % "\nat: " % stext % "\ndate: " % stext
    template name' Nothing = sformat noLocTemplate name' timeStr
    template name' (Just loc) = sformat withLocTemplate name' loc timeStr
