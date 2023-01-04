module AlisiaBasic where

import Control.Monad.Error (ErrorT)
import Data.Array (Array (..))
import Data.Complex (Complex (..))
import Data.IORef (IORef)
import Data.Ratio (Rational (..))
import System.IO (Handle)
import Text.ParserCombinators.Parsec (ParseError)


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Ratio Rational
             | Float Double
             | Complex (Complex Double)
             | String String
             | Char Char
             | Bool Bool
             | Vector (Array Int LispVal)
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String]
                    , vararg :: (Maybe String)
                    , body :: [LispVal]
                    , closure :: Env
                    }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle
             | Comment

data LispError = NumArgs Integer [LispVal]
               | ExpectCondClauses
               | ExpectCaseClauses
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError
type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ErrorT LispError IO
