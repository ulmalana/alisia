module AlisiaStringPrim where

import Control.Monad.Error

import AlisiaBasic

make_string :: [LispVal] -> ThrowsError LispVal
make_string [Number k, Char c] = return $ String $ replicate (fromIntegral k)  c
make_string badArgs = throwError $ TypeMismatch "int char" $ List badArgs

create_string :: [LispVal] -> ThrowsError LispVal
create_string xs
    | all isChar xs = return $ String $ foldr f "" xs 
    | otherwise = throwError $ TypeMismatch "lis karakter" $ List xs
  where
    isChar (Char _) = True
    isChar _ = False
    f (Char c) accum = c : accum

string_length :: [LispVal] -> ThrowsError LispVal
string_length [String s] = (return . Number . fromIntegral . length) s
string_length badArgs = throwError $ TypeMismatch "string" $ List badArgs

char_at :: [LispVal] -> ThrowsError LispVal
char_at [String s, Number n] = (return . Char) (s !! (fromIntegral n))
char_at badArgs = throwError $ TypeMismatch "(string number)" $ List badArgs

substring :: [LispVal] -> ThrowsError LispVal
substring [String s, Number start, Number end] =
    let start' = fromIntegral start
        end' = fromIntegral end
    in  (return . String) (drop start' $ take end' $ s)
substring badArgs = throwError $ TypeMismatch "(string number number)" $ List badArgs

string_append :: [LispVal] -> ThrowsError LispVal
string_append ss
    | all isString ss = (return . String . concat) $ map (\(String s) -> s) ss
    | otherwise = throwError $ TypeMismatch "lis string" $ List ss
  where
    isString (String _) = True
    isString _ = False 

