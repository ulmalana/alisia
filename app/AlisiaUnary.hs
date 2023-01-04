module AlisiaUnary where

import Data.Char (toLower)

import AlisiaBasic

not' (Bool x) = (Bool . not) x
not' _ = Bool False

boolP (Bool _) = Bool True
boolP _ = Bool False

listP (List _) = Bool True
listP (DottedList _ _) = Bool True
listP _ = Bool False

symbolP (Atom _) = Bool True
symbolP _ = Bool False

charP (Char _) = Bool True
charP _ = Bool False

stringP (String _) = Bool True
stringP _ = Bool False

vectorP (Vector _) = Bool True
vectorP _ = Bool False

symbol2string (Atom s) = String s
symbol2string _ = error "memerlukan atom"

string2symbol (String s) = Atom s
string2symbol _ = error "Memerlukan string"

ci_help :: (String -> String -> Bool) -> String -> String -> Bool
ci_help f a b = f (map toLower a) (map toLower b)

