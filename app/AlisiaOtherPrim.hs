module AlisiaOtherPrim where

import Control.Monad.Error
import AlisiaBasic
import AlisiaPrimHelper

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pasangan" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [x] y] = return x
cdr [DottedList (_:xs) y] = return $ DottedList xs y
cdr [badArg] = throwError $ TypeMismatch "pasangan" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs last] = return $ DottedList (x:xs) last
cons [x,y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool b1), (Bool b2)] = return $ Bool $ b1 == b2
eqv [(Number n1), (Number n2)] = return $ Bool $ n1 == n2
eqv [(String s1), (String s2)] = return $ Bool $ s1 == s2
eqv [(Atom a1), (Atom a2)] = return $ Bool $ a1 == a2

eqv [(DottedList xs x), (DottedList ys y)] = 
    eqv [List $ xs ++ [x], List $ ys ++ [y]]

eqv [(List l1), (List l2)]
    | length l1 /= length l2 = return $ Bool False 
    | otherwise = (return . Bool) $ all byPairs $ zip l1 l2 
  where byPairs (x,y) = case eqv [x,y] of 
                            Left err -> False 
                            Right (Bool val) -> val
-- eqv [(List l1), (List l2)] = return $ Bool $ (length l1 == length l2) &&
--                              (and $ map eqvPair $ zip l1 l2)
--     where eqvPair (x1, x2) = case eqv [x1, x2] of 
--                                 Left err -> False
--                                 Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [(List l1), (List l2)] = (return . Bool) $ all byPairs $ zip l1 l2
  where byPairs (x,y) = case equal [x,y] of
                             Left err -> False
                             Right (Bool val) -> val

equal [(DottedList xs x), (DottedList ys y)] =
    equal [List $ xs ++ [x], List $ ys ++ [y]]

equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)

equal badArgList = throwError $ NumArgs 2 badArgList
