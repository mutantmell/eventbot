{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Eventbot.Discord where

import           Data.Text (Text)
import qualified Data.Text as T

import qualified Network.Discord as D
import qualified Pipes as P

import Control.Monad
import Eventbot.Commands

reply :: D.Message -> Text -> P.Effect D.DiscordM ()
reply D.Message{D.messageChannel=chan} cont = D.fetch' $ D.CreateMessage chan cont Nothing

discord :: D.DiscordBot D.BotClient ()
discord = do
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
          (EventCommand (CreateEvent CreateEventData{..})) -> do
            reply msg "creating events..."
            pure ()
          (EventCommand InvalidEventCommand) -> do
            reply msg "Invalid command.  Help..."
            pure ()
