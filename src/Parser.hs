module Parser where

import Lang

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Text (Text, pack)
import Data.Void

type Parser = Parsec Void Text
type ParserError = ParseErrorBundle Text Void

blockComment :: Text -> Text -> Parser Text
blockComment start end = fmap pack $ string start >> manyTill anySingle (string end)

parserFlag :: Parser Expr
parserFlag = GetFlag <$ string "flag"

parserComment :: Parser Expr
parserComment = Comment <$> blockComment "/*" "*/"

parserSeq :: Parser Expr
parserSeq = fmap Seq $ parserExpr `sepBy1` string ";"

parserExpr :: Parser Expr
parserExpr = parserFlag
          <|> parserComment
          <|> parserSeq

parseExpr :: Text -> Either ParserError Expr
parseExpr = runParser (parserExpr <* eof) "User Input"


