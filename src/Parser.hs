module Parser where

import Lang

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

import Data.Text (Text, pack)
import Data.Void
import Data.Functor

type Parser = Parsec Void Text
type ParserError = ParseErrorBundle Text Void

blockComment :: Text -> Text -> Parser Text
blockComment start end = fmap pack $ string start >> manyTill anySingle (string end)

parserFlag :: Parser Expr
parserFlag = string "flag" >> spaceChar >> decimal <&> GetFlag

parserComment :: Parser Expr
parserComment = Comment <$> blockComment "<--" "-->"

parserSeq :: Parser [Expr]
parserSeq = do
  e <- parserFlag <|> parserComment
  space
  goon <- optional ";"
  space
  case goon of
    Just _ -> fmap (e:) parserSeq
    Nothing -> pure [e]

parserExpr :: Parser Expr
parserExpr = fmap Seq parserSeq

parseExpr :: Text -> Either ParserError Expr
parseExpr = runParser (parserExpr <* eof) "User Input"

textError :: ParserError -> Text
textError = pack . errorBundlePretty
