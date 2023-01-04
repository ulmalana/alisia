module AlisiaParsers where

import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readOct, readHex)
import Data.Char (toLower)
import Data.Array (listArray)
import Data.Ratio ((%))
import Data.Complex (Complex (..))
import Control.Monad.Except (throwError)

import AlisiaBasic

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "alisia" input of
                             Left err -> throwError $ Parser err
                             Right val -> return val

parseExpr :: Parser LispVal
parseExpr = parseComment
          <|> parseAtom
          <|> parseString
          <|> try parseBool
          <|> try parseChar
          <|> try parseComplex
          <|> try parseFloat
          <|> try parseRatio
          <|> try parseNumber
          <|> parseVector
          <|> parseQuoted
          <|> parseQuasiquote
          <|> try parseUnquoteSplicing
          <|> parseUnquote
          <|> parseList

parseComment :: Parser LispVal
parseComment = do many1 (char ';')
                  many1 (letter <|> digit <|> symbol <|> char ' ')
                  -- (char '\n')
                  return Comment

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               (return . Atom) (first:rest)

parseList :: Parser LispVal
parseList = char '(' >> parseList1

parseList1 :: Parser LispVal
parseList1 = (char ')' >> (return . List) []) 
               <|> do expr <- parseExpr
                      parseList2 [expr]

parseList2 :: [LispVal] -> Parser LispVal
parseList2 expr = (char ')' >> (return . List) (reverse expr)) 
                    <|> (spaces >> parseList3 expr)

parseList3 :: [LispVal] -> Parser LispVal
parseList3 expr = do char '.' >> spaces
                     dotted <- parseExpr
                     char ')'
                     return $ DottedList expr dotted
                  <|> do next <- parseExpr
                         parseList2 (next:expr)

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "kutip", x]

parseNumber :: Parser LispVal
parseNumber = parsePlainNumber <|> parseRadixNumber

parsePlainNumber :: Parser LispVal
parsePlainNumber = many1 digit >>= return . Number . read

parseRadixNumber :: Parser LispVal
parseRadixNumber = char '#' >> 
                   (
                        parseDecimal 
                        <|> parseBinary
                        <|> parseOctal
                        <|> parseHex
                   )

parseDecimal :: Parser LispVal
parseDecimal = do char 'd'
                  n <- many1 digit
                  (return . Number . read) n

parseBinary :: Parser LispVal
parseBinary = do char 'b'
                 n <- many $ oneOf "01"
                 (return . Number . bin2dig) n

parseOctal :: Parser LispVal
parseOctal = do char 'o'
                n <- many $ oneOf "01234567"
                (return . Number . (readWith readOct)) n

parseHex :: Parser LispVal
parseHex = do char 'x'
              n <- many $ oneOf "0123456789abcdefABCDEF"
              (return . Number . (readWith readHex)) n

parseRatio :: Parser LispVal
parseRatio = do num <- fmap read $ many1 digit
                char '/'
                denom <- fmap read $ many1 digit
                (return . Ratio) (num % denom)

parseFloat :: Parser LispVal
parseFloat = do whole <- many1 digit
                char '.'
                decimal <- many1 digit
                return $ Float (read (whole++"."++decimal))

parseComplex :: Parser LispVal
parseComplex = do r <- fmap toDouble (try parseFloat <|> parsePlainNumber)
                  char '+'
                  i <- fmap toDouble (try parseFloat <|> parsePlainNumber)
                  char 'i'
                  (return . Complex) (r :+ i)
               where toDouble (Float x) = x
                     toDouble (Number x) = fromIntegral x

parseString :: Parser LispVal
parseString = do char '"'
                 s <- many (escapedChars <|> (noneOf ['\\', '"']))
                 char '"'
                 (return . String) s

parseChar :: Parser LispVal
parseChar = do string "#\\"
               s <- many1 letter
               return $ case (map toLower s) of
                      "space" -> Char ' '
                      "newline" -> Char '\n'
                      [x] -> Char x

parseBool :: Parser LispVal
parseBool = do try (string "#be") <|> try (string "#sa")
               sisa <- many1 letter
               case sisa of
                 "nar" -> return $ Bool True
                 "lah" -> return $ Bool False
                 -- _ -> (parseDecimal <|> parseBinary <|> parseOctal <|> parseHex)

parseQuasiquote :: Parser LispVal
parseQuasiquote = do char '`'
                     expr <- parseExpr
                     return $ List [Atom "quasiquote", expr]

-- check later
parseUnquote :: Parser LispVal
parseUnquote = do char ','
                  expr <- parseExpr
                  return $ List [Atom "unquote", expr]

-- check later
parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do string ",@"
                          expr <- parseExpr
                          return $ List [Atom "unquote-splicing", expr]

parseVector :: Parser LispVal
parseVector = do string "#("
                 elems <- sepBy parseExpr spaces
                 char ')'
                 return $ Vector (listArray (0, (length elems)-1) elems)
                 

escapedChars :: Parser Char
escapedChars = do
             char '\\'
             c <- oneOf ['\\','"', 'n', 'r', 't']
             return $ case c of
                    '\\' -> c
                    '"'  -> c
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

bin2dig = bin2dig' 0

bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1)
                             in bin2dig' old xs


readWith :: (t -> [(a, b)]) -> t -> a
readWith f s = fst $ f s !! 0
