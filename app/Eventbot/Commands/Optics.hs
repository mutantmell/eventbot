{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Eventbot.Commands.Optics where

import           Control.Lens  ((^.), Iso')
import qualified Control.Lens as L
import           Data.Time (LocalTime, TimeZone, UTCTime)
import qualified Data.Time as Time

import qualified Eventbot.Google.Calendar as G

import           Eventbot.Commands

createEventIso :: TimeZone -> Iso' CreateEventData G.CalendarRequest
createEventIso tz = L.iso to from
  where
    tIso = utcLocalIso tz
    to CreateEventData{..} = G.CalendarRequest eventName (startTime ^. tIso) (endTime ^. tIso)
    from G.CalendarRequest{..} = CreateEventData eventName (startTime ^. L.from tIso) (endTime ^. L.from tIso)

utcLocalIso :: TimeZone -> Iso' LocalTime UTCTime
utcLocalIso tz = L.iso to from
  where
    to = Time.localTimeToUTC tz
    from = Time.utcToLocalTime tz
