{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lumibot.Web where

import Control.Monad.Trans
import Web.Scotty

import Lumibot.Commands

web = scotty 3000 $ do
  post "/" $ do
    token       <- param "token"
    teamId      <- param "team_id"
    channelId   <- param "channel_id"
    channelName <- param "channel_name"
    timestamp   <- param "timestamp"
    userId      <- param "user_id"
    userName    <- param "user_name"
    triggerWord <- param "trigger_word"

    text     <- param "text"
    let command = parseCommand text triggerWord

    response <- liftIO . handler $ SlackRequest command SlackData {..}
    json response
