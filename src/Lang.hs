module Lang where

import Prelude hiding (readFile, putStrLn)

import qualified Data.Text.IO as TIO
import Data.Text (Text, stripPrefix)
import System.Process
import qualified Data.Text as T

import Control.Concurrent (threadDelay)

class (Monad m) => MonadCommand m where
  readFile :: FilePath -> m Text
  delay :: Int -> m ()
  putStrLn :: Text -> m ()
  ls :: String -> m Text

instance MonadCommand IO where
  readFile = TIO.readFile
  delay = threadDelay
  putStrLn = TIO.putStrLn
  ls fp = fmap T.pack $ readProcess "/bin/ls" [fp] ""

data Expr = GetFlag | Comment Text | Seq [Expr]
  deriving (Show, Eq, Ord)


eval :: (MonadCommand m) => Expr -> m Text
eval GetFlag = readFile "./flag01.secret"
eval (Comment t) = pure t
eval (Seq exprs) = fmap T.concat . mapM eval $ exprs
