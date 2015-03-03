module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

-- Parser for
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
    deriving Show

-- Working off of this shit:
-- http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (noneOf "\"")
  _ <- char '"'
  return $ String x

-- Boom! Parses atoms.
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber

checkParse :: Show a => Either a LispVal -> String
checkParse p = case p of
  Left err -> "No match: " ++ show err
  Right v -> "Found value: " ++ show v

readExpr :: String -> String
readExpr = checkParse . parse parseExpr "lisp"

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr $ head args
