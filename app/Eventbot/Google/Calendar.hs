{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Eventbot.Google.Calendar where

import qualified Network.Google as Google
import qualified Network.Google.Auth as Google
import qualified Network.Google.AppsCalendar as Calendar

import Network.HTTP.Conduit (Manager)
import qualified Network.HTTP.Conduit as Conduit

import qualified Data.Foldable as F
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import qualified Data.Time as Time

import Control.Exception
import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Maybe

import GHC.Generics

data GoogleEnv = GoogleEnv
  { manager :: Conduit.Manager
  , logger :: Google.Logger
  , credentials :: (forall s . Google.Credentials s)
  }

data CalendarRequest = CalendarRequest
  { eventName :: Text
  , maybeLocation :: Maybe Text
  , startTime :: UTCTime
  , endTime :: UTCTime
  } deriving (Eq, Show, Generic)

data CalendarData = CalendarData
  { calendarId :: Text
  } deriving (Eq, Show, Generic)

data CalendarEvent = CalendarEvent
  { name :: Text
  , maybeLocation :: Maybe Text
  , startTime :: UTCTime
  , endTime :: UTCTime
  } deriving (Eq, Show, Generic)

data GoogleStateException = NoEventCalendarException deriving (Show, Eq)

instance Exception GoogleStateException

runCalendar :: GoogleEnv -> Google.Google '["https://www.googleapis.com/auth/calendar"] b -> IO b
runCalendar GoogleEnv{..} task = do
  env <- Google.newEnvWith credentials logger manager <&> (Google.envScopes .~ Calendar.calendarScope)
  Google.runResourceT . Google.runGoogle env $ task

getCalendar :: Text -> Google.Google '["https://www.googleapis.com/auth/calendar"] (Maybe CalendarData)
getCalendar name = do
  out <- Google.send Calendar.calendarListList
  let calendars = out ^.. Calendar.clItems . traverse
      maybeCalendar = F.find (\v -> v ^. Calendar.cleSummary == Just name) calendars
      maybeId = maybeCalendar >>= view Calendar.cleId
  pure $ CalendarData <$> maybeId

calendarRequestToGoogle :: CalendarData -> CalendarRequest -> Google.Google '["https://www.googleapis.com/auth/calendar"] (Calendar.Event)
calendarRequestToGoogle CalendarData{..} CalendarRequest{..} = Google.send request
  where
    request = Calendar.eventsInsert calendarId event
    event = Calendar.event & Calendar.eSummary .~ Just eventName
                           & Calendar.eStart .~ Just eventStart
                           & Calendar.eEnd .~ Just eventEnd
                           & Calendar.eLocation .~ maybeLocation
    eventStart = Calendar.eventDateTime & Calendar.edtDateTime .~ Just startTime
                                        & Calendar.edtTimeZone .~ Just "America/Los_Angeles"
    eventEnd = Calendar.eventDateTime & Calendar.edtDateTime .~ Just endTime
                                      & Calendar.edtTimeZone .~ Just "America/Los_Angeles"

getEventsFromDateGoogle :: UTCTime -> CalendarData -> Google.Google '["https://www.googleapis.com/auth/calendar"] [CalendarEvent]
getEventsFromDateGoogle fromTime CalendarData{..} = do
  response <- Google.send request
  let events = response ^. Calendar.eveItems
  pure $ mapMaybe mkEvent events
  where
    request = Calendar.eventsList calendarId & Calendar.elTimeMin .~ Just fromTime
    mkEvent :: Calendar.Event -> Maybe CalendarEvent
    mkEvent event = do
      let name = fromMaybe "<no name>" $ event ^. Calendar.eSummary
          location = event ^. Calendar.eLocation
      startTime <- event ^? Calendar.eStart . _Just . Calendar.edtDateTime . _Just
      endTime <- event ^? Calendar.eEnd . _Just . Calendar.edtDateTime . _Just
      pure $ CalendarEvent name location startTime endTime

