{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Eventbot.Commands
( Command(..)
, EventSubCommand(..)
, CreateEventData(..)

, parseCommand
, parseCommand'

) where
   
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (Day, LocalTime(..), TimeOfDay(..), TimeZone)
import qualified Data.Time as Time

import           Data.Attoparsec.Text ((<?>))
import qualified Data.Attoparsec.Text as P
import qualified Data.Char as C

import Control.Applicative
import Data.Either.Combinators
import Data.Functor
import Data.Maybe
import Data.Monoid
import GHC.Generics

data Command = EventCommand EventSubCommand
  deriving (Eq, Show, Generic)

data EventSubCommand = GetEvents
                     | CreateEvent CreateEventData
                     | InvalidEventCommand
  deriving (Eq, Show, Generic)

data CreateEventData = CreateEventData
  { eventName :: Text
  , startTime :: LocalTime
  , endTime :: LocalTime
  } deriving (Eq, Show, Generic)

data AMPM = AM | PM

parseCommand :: Text -> Maybe Command
parseCommand = rightToMaybe . P.parseOnly commandParser

parseCommand' :: Text -> Either String Command
parseCommand' = P.parseOnly commandParser

commandParser :: P.Parser Command
commandParser = EventCommand <$> eventCommandParser

eventCommandParser :: P.Parser EventSubCommand
eventCommandParser = do
  P.string "!event" $> ()
  P.skipSpace
  getEventsParser <|> createEventParser <|> invalidEventParser

getEventsParser :: P.Parser EventSubCommand
getEventsParser = P.string "all" $> GetEvents

createEventParser :: P.Parser EventSubCommand
createEventParser = do
  P.string "create"
  P.skipSpace
  CreateEvent <$> createEventDataParser

createEventDataParser :: P.Parser CreateEventData
createEventDataParser = shortFormEventData <|> longFormEventData

shortFormEventData :: P.Parser CreateEventData
shortFormEventData = do
  name <- nameParser
  P.skipSpace
  day <- dayParser
  P.skipSpace
  startTimeOfDay <- timeOfDayParser
  P.skipSpace
  endTimeOfDay <- timeOfDayParser
  let startTime = LocalTime day startTimeOfDay
      endTime = LocalTime day endTimeOfDay
  pure $ CreateEventData name startTime endTime

longFormEventData :: P.Parser CreateEventData
longFormEventData = do
  name <- nameParser
  P.skipSpace
  startTime <- localTimeParser
  P.skipSpace
  endTime <- localTimeParser
  pure $ CreateEventData name startTime endTime

localTimeParser :: P.Parser LocalTime
localTimeParser = parser <?> "localTimeParser"
  where 
    parser = do
      day <- dayParser
      P.skipSpace
      timeOfDay <- timeOfDayParser
      pure $ LocalTime day timeOfDay

dayParser :: P.Parser Day
dayParser = parser <?> "dayParser"
  where 
    parser = do
      month <- P.decimal
      dateSep
      day <- P.decimal
      dateSep
      year <- P.decimal
      let trueYear = if (year < 1000) then 2000 + year else year
          maybeDay = Time.fromGregorianValid trueYear month day
      maybe (fail "invalid date") pure maybeDay
    dateSep = P.satisfy (\c -> c == '-' || c == '/') $> ()

timeOfDayParser :: P.Parser TimeOfDay
timeOfDayParser = parser <?> "timeOfDayParser"
  where 
    parser = standard <|> military
    military = do
      hour <- P.decimal
      P.char ':' $> ()
      min <- P.decimal
      let maybeTime = Time.makeTimeOfDayValid hour min 0
      maybe (fail "invalid time of day") pure maybeTime
    standard = do
      hour <- P.decimal
      P.char ':' $> ()
      min <- P.decimal
      optional (P.char ' ') $> ()
      ampm <- P.string "AM" $> AM <|> P.string "PM" $> PM
      let trueHour = case (hour, ampm) of
            (12, AM) -> 0
            (12, PM) -> 12
            (n, AM) -> n
            (n, PM) -> n + 12
          maybeTime = Time.makeTimeOfDayValid trueHour min 0
      maybe (fail "invalid time of day") pure maybeTime
   
nameParser :: P.Parser Text
nameParser = parser <?> "nameParser"
  where
    parser = escapedString <|> P.takeWhile (not . C.isSpace)

escapedString :: P.Parser Text
escapedString = parser <?> "escapedStringParser"
  where
    parser = P.char '"' *> escapedString'
    escapedString' = do
      v <- P.takeWhile (\c -> c /= '"' && c /= '\\')
      rest <- P.anyChar >>= \case
        '\\' -> liftA2 (T.cons) P.anyChar escapedString'
        _    -> pure ""
      pure $ v <> rest

invalidEventParser :: P.Parser EventSubCommand
invalidEventParser = pure InvalidEventCommand
