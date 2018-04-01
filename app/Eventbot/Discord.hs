{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Eventbot.Discord
( discord

) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (Day, TimeZone, UTCTime)
import qualified Data.Time as Time

import qualified Network.Google.AppsCalendar as Calendar
import qualified Network.Discord as D
import qualified Pipes as P

import Data.String.Conversions

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid

import Eventbot.Commands
import Eventbot.Commands.Optics
import Eventbot.Google.Calendar


discord :: TimeZone -> GoogleEnv -> CalendarData -> D.DiscordBot D.BotClient ()
discord timeZone googleEnv calendar = do
  D.with D.ReadyEvent $ \(D.Init v u _ _ _) ->
    D.liftIO $ putStrLn $ "Connected to gateway v" ++ show v ++ " as user " ++ show u

  D.with D.MessageCreateEvent $ \msg@D.Message{..} -> do
    D.when (not $ D.userIsBot messageAuthor) $ do
      D.when ("!" `T.isPrefixOf` messageContent) $ do
        let maybeMsg = parseCommand messageContent
        forM_ maybeMsg $ \case
          (EventCommand (GetEvents getData)) -> do
            fromDay <- maybe (startOfToday timeZone) (pure . startOfDay timeZone) (maybeFromDay getData)
            let getRequest = getEventsFromDateGoogle fromDay calendar
            events <- liftIO $ runCalendar googleEnv getRequest
            let names = formatEvent timeZone <$> events
            mapM_ (reply msg) names
            pure ()
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
startOfDay timeZone day = Time.localTimeToUTC timeZone startOfDay
  where
    startOfDay = Time.LocalTime day Time.midnight

-- TODO: actually format
formatEvent :: TimeZone -> CalendarEvent -> Text
formatEvent tz = convertString . show

-- TODO: actually format
formatTime :: TimeZone -> UTCTime -> Text
formatTime tz = convertString . show
