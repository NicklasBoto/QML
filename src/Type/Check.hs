{-# LANGUAGE LambdaCase #-}

module Type.Check where

import Frontend.Intermediate.AST
import Frontend.QML.Par
import Type.Error
import Data.Complex
import Control.Monad.State
import Control.Monad.Except
import Frontend.Intermediate.Parser
import qualified Data.Map as Map

type Check = ExceptT TypeError (State Context)

type Context = Map.Map String Type

emptyContext :: Context
emptyContext = Map.empty

tcExpr :: String -> Either TypeError Type
tcExpr = runCheck . infer . parseExpr

tcFile :: FilePath -> IO (Either TypeError [Type])
tcFile path = runCheck . typecheck . parse <$> readFile path

runCheckWith :: Context -> Check a -> Either TypeError a
runCheckWith context check = evalState (runExceptT check) context

runCheck :: Check a -> Either TypeError a
runCheck = runCheckWith emptyContext

addToplevelTypes :: Program -> Check ()
addToplevelTypes = put . Map.fromList . map go
    where go :: Toplevel -> (String, Type)
          go (ToplF name _ _) = error $ name ++ " needs a type, for now..."
          go (ToplFT name _ typ _) = (name, typ)

typecheck :: Program -> Check [Type]
typecheck prog = addToplevelTypes prog
              >> mapM checkToplevel prog

checkToplevel :: Toplevel -> Check Type
checkToplevel (ToplF name args expr) = infer expr
checkToplevel (ToplFT name args expected_type expr) = mapM_ addArgTypes args
                                                   >> expr `has` expected_type

addArgTypes :: Arg -> Check ()
addArgTypes = modify . Map.union . Map.fromList

infer :: Expr -> Check Type
infer (Var n) = gets (Map.lookup n) >>= \case
    Nothing -> throwError $ VariableNotInScope n
    Just  t -> return t
infer QFalse = return TypeQubit
infer QTrue = return TypeQubit
infer (Tup []) = return TypeUnit
infer (Tup [e]) = infer e
infer (Tup (e:es)) = do
    e_type <- infer e
    es_type <- infer (Tup es)
    return $ e_type :* es_type
infer (Ifq c t f) = t <|> f >>= \case
    0 -> c `has` TypeQubit >> t `equals` f
    _ -> throwError $ NotOrthogonal t f
infer (If c t f) = c `has` TypeQubit >> t `equals` f
infer (Plus a b) = a `has` TypeQubit >> b `has` TypeQubit
infer (Mul _λ q) = q `has` TypeQubit
infer _ = error "not implemented"

equals :: Expr -> Expr -> Check Type
equals a b = do
    a_type <- infer a
    b_type <- infer b
    if a_type == b_type
        then return a_type
        else throwError $ TypeMismatch a_type b_type

has :: Expr -> Type -> Check Type
has e t = infer e >>= \case
    m | m == t    -> return t
      | otherwise -> throwError $ TypeMismatch t m

infixl 5 <|>
(<|>) :: Expr -> Expr -> Check (Complex Double)
a <|> b | a == b = return 1
QFalse <|> QTrue = return 0
QTrue <|> QFalse = return 0
Tup (t:t') <|> Tup (u:u') = (*) <$> t <|> u <*> Tup t' <|> Tup u'
Mul λ t <|> u = (conjugate λ *) <$> t <|> u
Plus t t' <|> u = (+) <$> t <|> u <*> t' <|> u
u <|> Mul λ t = (conjugate λ *) <$> t <|> u
u <|> Plus t t' = (+) <$> t <|> u <*> t' <|> u
a <|> b = throwError $ InnerProductNotDefined a b
