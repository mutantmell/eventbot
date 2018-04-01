{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Eventbot.Discord where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (TimeZone)

import qualified Network.Discord as D
import qualified Pipes as P

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid

import qualified Network.Google as Google
import qualified Network.Google.AppsCalendar as Calendar

import Eventbot.Commands
import Eventbot.Commands.Optics
import Eventbot.Google.Calendar

reply :: D.Message -> Text -> P.Effect D.DiscordM ()
reply D.Message{D.messageChannel=chan} cont = D.fetch' $ D.CreateMessage chan cont Nothing

discord :: TimeZone -> GoogleEnv -> CalendarData -> D.DiscordBot D.BotClient ()
discord timeZone googleEnv calendar = do
  D.with D.ReadyEvent $ \(D.Init v u _ _ _) ->
    D.liftIO $ putStrLn $ "Connected to gateway v" ++ show v ++ " as user " ++ show u

  D.with D.MessageCreateEvent $ \msg@D.Message{..} -> do
    D.when (not $ D.userIsBot messageAuthor) $ do
      D.when ("!" `T.isPrefixOf` messageContent) $ do
        let maybeMsg = parseCommand messageContent
        forM_ maybeMsg $ \case
          (EventCommand GetEvents) -> do
            reply msg "getting events..."
            pure ()
          (EventCommand (CreateEvent eventData)) -> do
            -- TODO: don't use gogol in non-google modules
            let calendarRequest = eventData ^. createEventIso timeZone
                insert = Google.send $ Calendar.eventsInsert (calendarId calendar) (calendarRequestToGoogle calendarRequest)
            liftIO $ runCalendar googleEnv insert
            reply msg $ "created event " <> Eventbot.Commands.eventName eventData
            pure ()
          (EventCommand InvalidEventCommand) -> do
            reply msg "Invalid command.  Help..."
            pure ()
