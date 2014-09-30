{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson (ToJSON (..), object, (.=))

import Web.Scotty

import Control.Monad.Trans

import Data.Text.Lazy (Text)
import Data.Monoid (mconcat)

data Command = Help

instance Parsable Command where
  parseParam _ = Right Help

data SlackRequest = SlackRequest Command SlackData

data SlackResponse = SuccessResponse Text
                   | FailResponse

instance ToJSON SlackResponse where
  toJSON (SuccessResponse t) = object ["text" .= t]
  toJSON FailResponse        = object ["text" .= failMsg]

failMsg :: Text
failMsg = "Sorry, I don't know that command. Try `@lumibot help`"


data SlackData = SlackData
  { token       :: Text
  , teamId      :: Text
  , channelId   :: Text
  , channelName :: Text
  , timestamp   :: Text
  , userId      :: Text
  , userName    :: Text
  , triggerWord :: Text
  }

main = scotty 3000 $ do
  post "/" $ do
    token       <- param "token"
    teamId      <- param "team_id"
    channelId   <- param "channel_id"
    channelName <- param "channel_name"
    timestamp   <- param "timestamp"
    userId      <- param "user_id"
    userName    <- param "user_name"
    triggerWord <- param "trigger_word"

    command     <- param "text"

    response <- liftIO . handler $ SlackRequest command SlackData {..}
    json response

handler :: SlackRequest -> IO SlackResponse
handler (SlackRequest Help _) = return $ SuccessResponse "This is the help message"
