module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show, Read)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

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
             let atom = first:rest
             return $ case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _    -> Atom atom


-- Original
-- parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit

parseNumber :: Parser LispVal
parseNumber = (many1 digit) >>= (\x -> return ((Number . read) x))

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr  "lisp" input of
                    Left err -> "No match: " ++ show err
                    Right v -> "Found value: " ++ show v

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)



