module AlisiaShow where

import Data.Ratio (numerator, denominator)

import AlisiaBasic

instance Show LispVal where show = showVal
instance Show LispError where show = showError

showVal :: LispVal -> String
showVal (String s) = "\"" ++ s ++ "\""
showVal (Atom name) = name
showVal (Number n) = show n
showVal (Float n) = show n
showVal (Ratio r) = (show $ numerator r) ++ "/" ++ (show $ denominator r)
showVal (Bool True) = "#benar"
showVal (Bool False) = "#salah"
showVal (Comment) = "komentar"
showVal (List xs) = "(" ++ unwordsList xs ++ ")"
showVal (Vector xs) = "TODO: show vektor"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Char c) = ['\'', c, '\'']
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params=args, vararg=varargs, body=body, closure=env}) =
    "(fungsi (" ++ unwords (map show args) ++
      (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

showError :: LispError -> String
showError ExpectCondClauses = "Percabangan memerlukan paling tidak 1 klausa bernilai benar"
showError ExpectCaseClauses = "Percabangan memerlukan paling tidak 1 klausa bernilai benar"
showError (UnboundVar msg varname) = msg ++ ": " ++ varname
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg func) = msg ++ ": " ++ show func

showError (NumArgs expected found) = "Memerlukan " ++ show expected
                                   ++ " argumen; Ditemukan: " ++ unwordsList found

showError (TypeMismatch expected found) = "Tipe invalid: memerlukan " ++ expected
                                   ++ ", ditemukan " ++ show found

showError (Parser parseErr) = "Galat saat parsing di " ++ show parseErr

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
