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
import           Data.Time (LocalTime, TimeOfDay, TimeZone)
import qualified Data.Time as Time

import Control.Exception
import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class

import GHC.Generics

data GoogleEnv = GoogleEnv
  { manager :: Conduit.Manager
  , logger :: Google.Logger
  , credentials :: (forall s . Google.Credentials s)
  }

data CalendarData = CalendarData
  { calendarId :: Text
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

data CalendarRequest = CalendarRequest
  { eventName :: Text
  , startTime :: LocalTime
  , endTime :: LocalTime
  } deriving (Eq, Show, Generic)

calendarRequestToGoogle :: CalendarRequest -> TimeZone -> Calendar.Event
calendarRequestToGoogle CalendarRequest{..} timeZone = event
  where
    startTimeUtc = Time.localTimeToUTC timeZone startTime
    endTimeUtc = Time.localTimeToUTC timeZone endTime
    eventStart = Calendar.eventDateTime & Calendar.edtDateTime .~ Just startTimeUtc
                                        & Calendar.edtTimeZone .~ Just "America/Los_Angeles"
    eventEnd = Calendar.eventDateTime & Calendar.edtDateTime .~ Just endTimeUtc
                                      & Calendar.edtTimeZone .~ Just "America/Los_Angeles"
    event = Calendar.event & Calendar.eSummary .~ Just eventName
                           & Calendar.eStart .~ Just eventStart
                           & Calendar.eEnd .~ Just eventEnd
