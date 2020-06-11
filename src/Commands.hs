{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Commands where

import qualified Data.HashMap as HM
import Data.Text (Text)
import qualified Data.Text as T

import Lang

newtype Command = Command { unCommand :: forall m. (MonadCommand m) => Text -> m Text }

pureCommand :: (Text -> Text) -> Command
pureCommand f = Command $ pure . f

txtCommand :: Text -> Command
txtCommand = pureCommand . const

commands :: HM.Map Text (Command, Text)
commands = HM.fromList
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

