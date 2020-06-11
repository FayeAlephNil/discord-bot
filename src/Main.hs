import Prelude hiding (readFile, putStrLn)

import Data.Text (stripPrefix, Text)
import Data.Text.Manipulate (breakWord)

import Discord
import Discord.Types
import qualified Discord.Requests as R
import qualified Data.HashMap as HM

import Lang
import Commands

ourbot :: IO ()
ourbot = do
  tok <- readFile "./auth-token.secret"
  userFacingError <- runDiscord $ def { discordToken = tok
                                        , discordOnEvent = eventHandler }
  putStrLn userFacingError

handleError :: RestCallErrorCode -> IO ()
handleError _ = pure ()

runCommand :: DiscordHandle -> Message -> Command -> IO ()
runCommand dis m (Command command) = if fromBot m
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
eventHandler _ _ = pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

main :: IO ()
main = ourbot
