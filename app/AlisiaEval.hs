module AlisiaEval where

import Control.Monad (liftM)
import Control.Monad.Except (throwError, liftIO, catchError)
import System.IO
import Data.IORef

import AlisiaBasic
import AlisiaVar
import AlisiaShow (showVal)
import AlisiaParsers
import AlisiaOtherPrim (eqv)
import AlisiaPrimitives

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env Comment = return Comment
eval env val@(String _) = return val
eval env val@(Char _) = return val
eval env val@(Number _) = return val
eval env val@(Float _) = return val
eval env val@(Ratio _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "kutip", val]) = return val
eval env (List [Atom "tampilkan", val]) = eval env val
eval env (List (Atom "tampilkan" : xs)) = liftM head $ mapM (eval env) xs -- make this to walk on every arg, not the first arg
eval env (List [Atom "jika", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        Bool True -> eval env conseq
        otherwise  -> throwError $ TypeMismatch "boolean" result
eval env (List (Atom "cabang" : [])) = throwError ExpectCondClauses
eval env (List (Atom "cabang" : cs)) = evalConds env cs
eval env (List (Atom "kasus" : [])) = throwError ExpectCaseClauses

eval env (List (Atom "kasus" : key : cs)) = do
    keyVal <- eval env key
    evalCaseCases env keyVal cs

eval env (List [Atom "set", Atom var, form]) =
    eval env form >>= setVar env var

eval env (List [Atom "definisi", Atom var, form]) =
    eval env form >>= defineVar env var

eval env (List (Atom "definisi" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var

eval env (List (Atom "definisi" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var

eval env (List (Atom "fungsi" : List params : body)) =
    makeNormalFunc env params body

eval env (List (Atom "fungsi" : DottedList params varargs : body)) =
    makeVarargs varargs env params body

eval env (List (Atom "fungsi" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body

eval env (List [Atom "muat", String filename]) = load filename >>= liftM last . mapM (eval env)

eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals

eval env badForm = throwError $ BadSpecialForm "Bentuk khusus tak dikenali" badForm

-- helpers

evalConds :: Env -> [LispVal] -> IOThrowsError LispVal
evalConds env (List (Atom "lain" : xs) : []) = evalCondElse env xs
evalConds _ [] = throwError ExpectCondClauses
evalConds env (List clause : cs) = evalCondClause env clause cs
evalConds _ badClauses = throwError $ TypeMismatch "klausa cabang" $ List badClauses

evalCondClause env (test : xs) rest = do
    result <- eval env test
    case result of
         Bool False -> evalConds env rest
         Bool True -> trueDo xs
         otherwise -> throwError $ TypeMismatch "boolean" result
  where 
    trueDo [] = return $ Bool True
    trueDo xs = evalToLast env xs

evalCondElse :: Env -> [LispVal] -> IOThrowsError LispVal
evalCondElse _ [] = throwError ExpectCondClauses
evalCondElse env xs = evalToLast env xs

evalCaseCases :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalCaseCases _ _ [] = throwError ExpectCaseClauses
evalCaseCases env _ [List (Atom "lain" : cExprs)] = evalToLast env cExprs
evalCaseCases env key ((List ((List cKeys) : cExprs)) : cs) = do
    let result = any anyOf $ map (\x -> eqv [key, x]) cKeys
    case result of
        False -> evalCaseCases env key cs
        True -> evalToLast env cExprs
  where
    anyOf (Right (Bool True)) = True
    anyOf _ = False
evalCaseCases _ _ _ = throwError ExpectCaseClauses

evalToLast :: Env -> [LispVal] -> IOThrowsError LispVal
evalToLast _ [] = throwError $ NumArgs 1 []
evalToLast env xs = liftM last $ mapM (eval env) xs

--- 

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

nullEnv :: IO Env
nullEnv = newIORef []

---

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives)
  where makeFunc constructor (var, func) = (var, constructor func)

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal

---

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs arg env = case arg of
              Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
              Nothing -> return env
apply (IOFunc func) args = func args

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("terapkan", applyProc),
               ("buka-berkas-input", makePort ReadMode),
               ("buka-berkas-output", makePort WriteMode),
               ("tutup--port-input", closePort),
               ("tutup-port-output", closePort),
               ("baca", readProc),
               ("tulis", writeProc),
               ("baca-konten", readContents),
               ("baca-semua", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

