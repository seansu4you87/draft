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
  show = showVal
  -- show (Atom string) = "Atom: " ++ string
  -- show (List list) = "List: " ++ show (map show list)
  -- show (DottedList list val) = "DottedList: " ++ show list ++ " " ++ show val
  -- show (Number int) = "Number: " ++ show int
  -- show (String string) = "String: " ++ string
  -- show (Bool bool) = "Bool: " ++ show bool
  -- show _ = "wtf"

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- EVALUATOR --

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val

-- PARSER --

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
-- parseNumber :: Parser LispVal
parseNumber = do
  number <- many1 digit
  return $ Number . read $ number
-- Couldn't figure out how to implement with >>= =[
-- parseNumber :: Parser LispVal
-- parseNumber = (many1 digit) >>= (Number . read)

-- parseList :: Parser LispVal
-- parseList = liftM List $ sepBy parseExpr spaces
parseList :: Parser LispVal
parseList = do
  list <- sepBy parseExpr spaces
  return $ List list

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do
          char '('
          x <- (try parseList) <|> parseDottedList
          char ')'
          return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

-- MAIN --

-- main :: IO ()
-- main = do
--   args <- getArgs
--   mapM_ putStrLn args
--   putStrLn (readExpr (args !! 0))
main :: IO ()
main = getArgs >>= putStrLn . show. eval . readExpr . (!! 0)
