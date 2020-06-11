{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Commands where

import Prelude hiding (readFile, putStrLn)

import qualified Data.HashMap as HM
import Data.Text (Text)
import qualified Data.Text as T

import Filesystem.Path.CurrentOS (fromText)

import Data.Either.Combinators (mapBoth)
import Data.Maybe

import Data.Text.ICU (Regex)
import qualified Data.Text.ICU as ICU

import Lang
import Parser

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
  , ("eval", (evalc, "Evaluates code in the DSL for CTF challenges"))
  , ("ls", (Command $ ls . T.unpack, "Lists files in a directory"))
  , ("tryfile", (tryFile, "Try a file for the second flag"))
  ]

fullHelp :: Text
fullHelp = T.unlines $ fmap someGlue $ HM.toList $ fmap snd commands
  where
    someGlue (t1, t2) = t1 <> ": " <> t2

help :: Text -> Text
help "" = fullHelp
help name = case name `HM.lookup` commands of
  Just (_, h) -> name <> ": " <> h
  Nothing -> "Error: {" <> name <> "}" <> " is not a command name"

fromEither :: Either a a -> a
fromEither (Left a) = a
fromEither (Right a) = a

removeRegex :: Regex -> Text -> Text
removeRegex r t = removal t matchGroups
  where
    matchGroups = foldMap (ICU.unfold ICU.group) $ ICU.findAll r t
    removal = foldl $ flip removeOne
    removeOne needle = T.replace needle ""

untilFix :: (Eq a) => (a -> a) -> a -> a
untilFix f a = if f a == a then a else untilFix f (f a)

flag04Remove :: Text -> Text
flag04Remove = untilFix $ removeRegex "flag\\s+0*4"

sanitize :: Text -> Text
sanitize t = "<--" <> newtxt <> "-->"
  where
    newtxt = flag04Remove $ removeRegex "flag\\s+3" t 

evalc :: Command
evalc = Command $ fromEither . mapBoth (pure . textError) eval . parseExpr . sanitize . T.toLower

checkFile :: Text -> Text
checkFile t = fromMaybe "This is the wrong file" $ "This is the right file 9b3a163513f2da45e527a09d6c42d24f: " `T.stripPrefix` t

tryFile :: Command
tryFile = Command $ fmap checkFile . readFile . T.unpack
