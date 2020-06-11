module Lang where

import Prelude hiding (readFile, putStrLn)

import qualified Data.Text.IO as TIO
import Data.Text (Text)
import qualified Data.Text as T

import Control.Concurrent (threadDelay)

class (Monad m) => MonadCommand m where
  readFile :: FilePath -> m Text
  delay :: Int -> m ()
  putStrLn :: Text -> m ()

instance MonadCommand IO where
  readFile = TIO.readFile
  delay = threadDelay
  putStrLn = TIO.putStrLn

data Expr = GetFlag | Comment Text | Seq [Expr]
  deriving (Show, Eq, Ord)

eval :: (MonadCommand m) => Expr -> m Text
eval GetFlag = readFile "./auth-token.secret"
eval (Comment t) = pure t
eval (Seq exprs) = fmap T.concat . mapM eval $ exprs
