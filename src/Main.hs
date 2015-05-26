module Main where

import Options.Applicative

import Anne.Web

-- | Define what options we want and in which order to feed the results of
-- those arguments into the WebConfig type which we get from Anne.Web
webConfig :: Parser WebConfig
webConfig = WebConfig
  <$> strOption
      ( long "slack-token"
     <> short 't'
     <> help "The Slack API Token for the Outgoing Webhook" )
  <*> option auto
      ( long "port"
     <> short 'p'
     <> help "The port for the webserver to listen to" )

-- | The main function in our executable which includes a bit of helpful
-- description about what the executable does. Parsers a WebConfig from the
-- options and feeds that into `web`.
main :: IO ()
main = execParser opts >>= web where
  opts = info (helper <*> webConfig)
    ( fullDesc
   <> progDesc ( "Start a webserver for Anne, to be" <>
                 "used with Outgoing Webhooks in Slack" )
   <> header "anne - a slack bot for learning Haskell" )

