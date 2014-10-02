{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lumibot.Web where

import Data.Aeson (object, (.=))
import Data.Text.Lazy (unpack)
import Control.Monad.Trans
import Network.HTTP.Types
import Web.Scotty

import Lumibot.Commands

-- | The configuration parameters we want for our webserver.
data WebConfig = WebConfig
  { configToken :: String -- ^ The Token we get from Slack to authenticate
  , configPort  :: Int    -- ^ The port we want to launch our web server on
  }

-- | This is the main spinner for out web application. It reads all the
-- parameters that are fed into `SlackData` (by the use of the
-- RecordWildCards language pragma) as well as the text from which we want
-- to parse a command. We handle the resulting command and feed that into
-- a response. The response is 200 is the `configToken` matches the one we
-- got from the parameters, otherwise we give a 403 with an error message.
web :: WebConfig -> IO ()
web WebConfig {..} = scotty configPort $ do
  post "/" $ do
    teamId      <- param "team_id"
    channelId   <- param "channel_id"
    channelName <- param "channel_name"
    timestamp   <- param "timestamp"
    userId      <- param "user_id"
    userName    <- param "user_name"
    triggerWord <- param "trigger_word"

    text       <- param "text"
    let command = parseCommand text triggerWord
    response <- liftIO . handler $ SlackRequest command SlackData {..}

    token <- param "token"
    if configToken == token
      then json response
      else do
        status status403
        json $ object ["error" .= ("Your Slack token was incorrect." :: String)]
