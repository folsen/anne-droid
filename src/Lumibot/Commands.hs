{-# LANGUAGE OverloadedStrings #-}

module Lumibot.Commands where

import System.Random
import Data.Text.Lazy (Text, unpack)
import Data.Aeson
import Web.Scotty (Parsable, parseParam)

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Char

-- | The top-level SlackRequest, this is what is coming in from the Slack.
-- We have a Command and some SlackData. The Command is defined below along
-- with how to parse text into any command. The SlackRequest is what is
-- given to our handler so whatever is in the command along with auxilliary
-- data found in SlackData is what is at our disposal to handle the
-- commands.
data SlackRequest = SlackRequest Command SlackData

-- | The auxilliary data coming in from slack.
data SlackData = SlackData
  { teamId      :: Text -- ^ ID for the `team` this is being sent from
  , channelId   :: Text -- ^ ID for the channel this is being sent from
  , channelName :: Text -- ^ Name for the channel this is being sent from
  , timestamp   :: Text -- ^ Unix timestamp for message
  , userId      :: Text -- ^ ID for the user this is being sent from
  , userName    :: Text -- ^ Name for the user this is being sent from
  , triggerWord :: Text -- ^ The trigger word that was used to send this message
                        --   The trigger word is defined in the slack
                        --   outgoing hook.
  }

-- | These are the commands we have available.
-- To add a new command here, just add a line with | in front, call your
-- command something and feel free to add parameters to the command that
-- you can extract from the message in the parser.
data Command = Help
             | CommandNotFound
             deriving (Show)

-- | Top level parser, this lists up all of the available command-parsers
-- and puts them in the order in which we want to try commands.
commandParser :: CommandParser
commandParser = choice
  [ parseHelpCommand
  ]

-- | A parser for the Help command.
-- This command is used by writing `@lumibot help` so we just want to
-- parse that the word help is there (and first in the message), it can
-- optionally be followed something else after at least 1 space. This
-- allows us to parse any further commands after should we want to in the
-- future, such as getting help on some other command.
parseHelpCommand :: CommandParser
parseHelpCommand = do
  string "help"
  eof <|> skipMany1 space
  return Help

-- | Type signature for the handler. Note that this is all done in the IO
-- monad, I made this choice because we might very well want to do things
-- like make HTTP requests, read the time or otherwise reach out into the
-- outside world when we are handling commands.
handler :: SlackRequest -> IO SlackResponse

-- | Handler for the Help command. Just print out what commands we have
-- available, this text needs to be updated as we expand.
handler (SlackRequest Help _) = return $
  SuccessResponse "This is the help message"

-- | This functions handles the CommandNotFound by creating a random number (an
-- IO action) and picking an item from the funnyFails list. This is not very
-- pretty, and would fail if the funnyFails list was empty! Though we know it's
-- not since we are hard-coding it.
handler (SlackRequest CommandNotFound _) = do
  elem <- randomRIO (0, length funnyFails - 1)
  return . FailResponse $ funnyFails !! elem

-- | A list of funny fail messages when someone has entered a command that
-- doesn't exist.
funnyFails :: [Text]
funnyFails =
  [ "Sorry, I don't know that command. Try `@lumibot help`"
  , "Hmm.. not sure what you're talking about. Try `@lumibot help`?"
  ]

--------------------------------------------------------------------------------
-- The stuff below here are just some minor necessary parts to hook things
-- up. Don't mind this stuff too much for now.

-- | A simple type synonym to call our parser something nice like
-- `CommandParser`
type CommandParser = Parsec Text () Command

-- | The top-level type of response we want to give back to Slack. This
-- response will be converted to JSON and sent back to Slack.
data SlackResponse = SuccessResponse Text
                   | FailResponse Text

-- | This defines how we convert our Haskell data-type to JSON. Basically
-- Slack only takes back a 200 success response with a `text` element, so we
-- basically render these two the same. However, I think they do accept
-- some auxilliary parameters like the picture to use for the response so
-- we might want to look deeper into how to format responses differently
-- for successes and failures.
instance ToJSON SlackResponse where
  toJSON (SuccessResponse t) = object ["text" .= t]
  toJSON (FailResponse t)    = object ["text" .= t]

-- | This is a helper function to use in the web request handler that will
-- run our `CommandParser` on the input text, it will additionally run
-- `commandWithoutTrigger` to remove the trigger word from the message so
-- that we're only left with the relevant part for processing our commands.
parseCommand :: Text -> Text -> Command
parseCommand msg trigger =
  case parse (commandWithoutTrigger trigger) "" msg of
    Left _  -> CommandNotFound
    Right c -> c

-- | The commandWithoutTrigger function is a `CommandParser` that takes an
-- additional argument of a trigger word. We parse the trigger, skip 0 or
-- more spaces and then run the main `commandParser` on the rest of the
-- input. We also allow an optional semi-colon after the trigger word.
commandWithoutTrigger :: Text -> CommandParser
commandWithoutTrigger trigger = do
  string $ unpack trigger
  optional $ char ':'
  skipMany space
  commandParser

