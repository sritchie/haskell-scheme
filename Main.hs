-- Working off of this shit:
-- http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

module Main where
import Control.Applicative ((<$>))
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readOct, readDec, readHex, readInt)
import Data.Char

-- We'l start with the basic parsers. Symbol and a whitespace
-- clone. The `spaces` function is probably present already as lexeme
-- or something like that.
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- The next parsers deal with values in the Scheme that I'm
-- writing. Here are the data types:
data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Char Char
  | Bool Bool
    deriving Show

-- Characters that can be escaped in Scheme.
escapeChars :: String
escapeChars = "\\\"rntbf"

-- This guy matches if it encounters a backslash followed by one of
-- the approved escape characters above. Returns the escaped string.
escape :: Parser String
escape = do
  bs <- char '\\'
  c  <- oneOf escapeChars
  return [bs,c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\""

-- woah, fmap return works because a string is a sequence of
-- characters. nonEscape is a character parser, but return pulls the
-- character up into the list Monad.
character :: Parser String
character = fmap return nonEscape <|> escape

bracketed :: Char -> Parser a -> Char -> Parser a
bracketed before p after = do
  _ <- char before
  matches <- p
  _ <- char after
  return matches

-- Modified internal parser. This thing can handle escaped shit.
parseString :: Parser LispVal
parseString = (String . concat) <$> bracketed '"' (many character) '"'

-- Boom! Parses atoms.
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return $ Atom (first:rest)

parseBool :: Parser LispVal
parseBool = do
  _ <- char '#'
  s <- oneOf "tf"
  return $ case s of
    't' -> Bool True
    'f' -> Bool False

-- Parses a number.
parseNumber0 :: Parser LispVal
parseNumber0 = liftM (Number . read) (many1 digit)

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

binDigit :: Parser Char
binDigit = oneOf "01"

isBinDigit :: Char -> Bool
isBinDigit c = c == '0' || c == '1'

readBin :: ReadS Integer
readBin = readInt 2 isBinDigit digitToInt

parseDigits :: Char -> Parser String
parseDigits b = many1 d where
  d = case b of
       'b' -> octDigit
       'o' -> octDigit
       'd' -> digit
       'x' -> hexDigit

parseHash :: Parser LispVal
parseHash = do
  _ <- oneOf "#"
  base <- oneOf "bodx"
  digits <- parseDigits base
  let reader = case base of
        'b' -> readBin
        'o' -> readOct
        'd' -> readDec
        'x' -> readHex
  let ((d, _):_) = reader digits
  return (Number d)

parseNormalNum :: Parser LispVal
parseNormalNum = liftM (Number . read) (many1 digit)

parseNumber :: Parser LispVal
parseNumber = parseNormalNum <|> parseHash

parseChar :: Parser LispVal
parseChar = do
  _ <- string "#\\"
  xs <- string "space" <|> string "newline" <|> fmap return anyChar
  return $ case (map toLower xs) of
    "space" -> Char ' '
    "newline" -> Char '\n'
    [x] -> Char x

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> try parseNumber
            <|> try parseChar
            <|> try parseBool

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
