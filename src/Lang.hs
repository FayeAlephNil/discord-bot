module Lang where

import Prelude hiding (readFile, putStrLn)

import qualified Data.Text.IO as TIO
import Data.Text (Text, stripPrefix, pack)
import System.Process
import System.Directory (doesFileExist, doesPathExist)
import qualified Data.Text as T

import Control.Concurrent (threadDelay)

class (Monad m) => MonadCommand m where
  readFile :: FilePath -> m Text
  delay :: Int -> m ()
  putStrLn :: Text -> m ()
  ls :: String -> m Text

instance MonadCommand IO where
  readFile fp = do
    b <- doesFileExist fp
    if b
      then TIO.readFile fp
      else pure $ "Error: The file {" <> pack fp <> "} does not exist"
  delay = threadDelay
  putStrLn = TIO.putStrLn
  ls fp = do
    b <- doesPathExist fp
    if b
      then T.pack <$> readProcess "/bin/ls" (words fp) ""
      else pure $ "Error: The path {" <> pack fp <> "} does not exist"

data Expr = GetFlag Int | Comment Text | Seq [Expr]
  deriving (Show, Eq, Ord)

zeropad :: Int -> Int -> String
zeropad len z =
  let txt = show z
      offset = len - length txt
      toadd = replicate (if offset >= 0 then offset else 0) '0'
  in toadd <> txt

eval :: (MonadCommand m) => Expr -> m Text
eval (GetFlag i) = readFile $ "./flag" <> zeropad 2 i <> ".secret"
eval (Comment t) = pure t
eval (Seq exprs) = fmap T.concat . mapM eval $ exprs
