{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}

import Prelude hiding (readFile)

import Control.Monad (when)

import Data.Text (stripPrefix, isPrefixOf, toLower, Text)
import qualified Data.Text as T
import Data.Text.Manipulate (breakWord)

import Control.Concurrent (threadDelay)
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import qualified Discord.Requests as R
import qualified Data.HashMap as HM

import Data.Semigroup

newtype Command = Command { unCommand :: forall m. (MonadCommand m) => Text -> m Text }

class (Monad m) => MonadCommand m where
  readFile :: FilePath -> m Text
  delay :: Int -> m ()

instance MonadCommand IO where
  readFile = TIO.readFile
  delay = threadDelay

-- | Replies "pong" to every message that starts with "ping"
ourbot :: IO ()
ourbot = do
  tok <- readFile "./auth-token.secret"
  userFacingError <- runDiscord $ def { discordToken = tok
                                        , discordOnEvent = eventHandler }
  TIO.putStrLn userFacingError

handleError :: RestCallErrorCode -> IO ()
handleError _ = pure ()

pureCommand :: (Text -> Text) -> Command
pureCommand f = Command $ pure . f

txtCommand :: Text -> Command
txtCommand = pureCommand . const

commands :: HM.Map Text (Command, Text)
commands = HM.fromList $
  [
    ("cat", (pureCommand id, "A command that copies what you say"))
  , ("help", (pureCommand help, "Outputs documentation about the bot"))
  , ("github", (txtCommand "https://github.com/FayeAlephNil/discord-bot", "Gives the bots github source code!"))
  ]

fullHelp :: Text
fullHelp = T.unlines $ fmap someGlue $ HM.toList $ fmap snd commands
  where
    someGlue (t1, t2) = t1 <> ": " <> t2

help :: Text -> Text
help "" = fullHelp
help name = case name `HM.lookup` commands of
  Just (_, h) -> name <> ": " <> h
  Nothing -> "{" <> name <> "}" <> " is not a command name"

runCommand :: DiscordHandle -> Message -> Command -> IO ()
runCommand dis m (Command command) = if (fromBot m)
  then pure ()
  else do
      t <- command (messageText m)
      x <- restCall dis $ R.CreateMessage (messageChannel m) t
      case x of
        Left er -> handleError er
        _ -> pure ()

runCommandName :: DiscordHandle -> Message -> Text -> IO ()
runCommandName dis m name = case HM.lookup name commands of
  Just (c, _) -> runCommand dis m c
  Nothing -> do
    _ <- restCall dis $ R.CreateMessage (messageChannel m) "Command not found"
    pure ()

prefix :: Text
prefix = "f."

stripCommand :: Text -> Maybe (Text, Text)
stripCommand = fmap breakWord . stripPrefix prefix 

eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis (MessageCreate m) = case stripCommand (messageText m) of
  Just (cname, args) -> let
    newMsg = m {messageText = args}
    in runCommandName dis newMsg cname
  Nothing -> pure ()
eventHandler dis _ = pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

main :: IO ()
main = ourbot
