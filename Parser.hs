module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where
  show (Atom string) = "Atom: " ++ string
  show (List list) = "List: " ++ show (map show list)
  show (DottedList list val) = "DottedList: " ++ show list ++ " " ++ show val
  show (Number int) = "Number: " ++ show int
  show (String string) = "String: " ++ string
  show (Bool bool) = "Bool: " ++ show bool
  -- show _ = "wtf"

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = [first] ++ rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    otherwise -> Atom atom

-- parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
parseNumber :: Parser LispVal
parseNumber = do
  number <- many1 digit
  return $ Number . read $ number

-- Couldn't figure out how to implement with >>= =[
-- parseNumber :: Parser LispVal
-- parseNumber = (many1 digit) >>= (Number . read)

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

main :: IO ()
main = do
  args <- getArgs
  mapM_ putStrLn args
  putStrLn (readExpr (args !! 0))
