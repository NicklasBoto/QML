module Frontend.QML.Skel where

-- Haskell module generated by the BNF converter

import Frontend.QML.Abs
import Frontend.QML.ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transScalar :: Scalar -> Result
transScalar x = case x of
  Scalar string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  Progr toplevels -> failure x
transToplevel :: Toplevel -> Result
transToplevel x = case x of
  ToplF ident args expr -> failure x
  ToplFT ident args type_ expr -> failure x
transArg :: Arg -> Result
transArg x = case x of
  ArgTup typedvars -> failure x
transTypedVar :: TypedVar -> Result
transTypedVar x = case x of
  TVar ident type_ -> failure x
transExpr :: Expr -> Result
transExpr x = case x of
  EVar ident -> failure x
  ETrue -> failure x
  EFalse -> failure x
  ETup exprs -> failure x
  EMul complex expr -> failure x
  EApp expr1 expr2 -> failure x
  EPlus expr1 expr2 -> failure x
  EIfq expr1 expr2 expr3 -> failure x
  EIf expr1 expr2 expr3 -> failure x
  ELet lets expr -> failure x
transLet :: Let -> Result
transLet x = case x of
  LLet pattern expr -> failure x
transComplex :: Complex -> Result
transComplex x = case x of
  CComp scalar1 scalar2 -> failure x
transPattern :: Pattern -> Result
transPattern x = case x of
  PVar ident -> failure x
  PTup ident1 ident2 -> failure x
transType :: Type -> Result
transType x = case x of
  TQubit -> failure x
  TUnit -> failure x
  TTens type_1 type_2 -> failure x
  TArrow type_1 type_2 -> failure x

