-- Working off of this shit:
-- http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

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

escape :: Parser String
escape = do
  bs <- char '\\'
  c  <- oneOf "\\\"rntbf"
  return [bs,c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\""

-- woah, fmap return works because
character :: Parser String
character = fmap return nonEscape <|> escape

-- Modified internal parser. This thing can handle escaped shit.
parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  strings <- many character
  _ <- char '"'
  return $ String $ concat strings

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

-- Parses a number.
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) (many1 digit)

-- This is the same as parseNumber above, just written using do
-- notation.
parseNumber1 :: Parser LispVal
parseNumber1 = do
  numString <- many1 digit
  let num = read numString
  return (Number num)

-- Same thing here, written using >>= to explicitly thread the
-- argument on through.
parseNumber2 :: Parser LispVal
parseNumber2 = many1 digit >>= (return . Number . read)

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
