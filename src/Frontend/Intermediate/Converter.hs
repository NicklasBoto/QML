module Frontend.Intermediate.Converter 
    ( -- * Converters
      convertProgram
    , convertToplevel
    , convertArg
    , convertExpr
    , convertType
    ) where

import Data.Complex ( Complex )
import Frontend.Intermediate.AST
    ( Type(..),
      Pattern(..),
      Let(LLet),
      Expr(..),
      Arg(..),
      Toplevel(..),
      Program )
import qualified Frontend.QML.Abs as Abs

convertProgram :: Abs.Program -> Program
convertProgram (Abs.Progr p) = map convertToplevel p

convertToplevel :: Abs.Toplevel -> Toplevel
convertToplevel (Abs.ToplF (Abs.Ident f) as e) = ToplF f (map convertArg as) (convertExpr e)
convertToplevel (Abs.ToplFT (Abs.Ident f) as t e) = ToplFT f (map convertArg as) (convertType t) (convertExpr e)

convertArg :: Abs.Arg -> Arg
convertArg (Abs.ArgTup tvs) = map convertTVar tvs
  where convertTVar :: Abs.TypedVar -> (String, Type)
        convertTVar (Abs.TVar (Abs.Ident n) t) = (n, convertType t)

convertExpr :: Abs.Expr -> Expr
convertExpr (Abs.EVar (Abs.Ident s)) = Var s
convertExpr Abs.ETrue = QTrue
convertExpr Abs.EFalse = QFalse
convertExpr (Abs.ETup ts) = Tup $ map convertExpr ts
convertExpr (Abs.EMul c e) = Mul (convertComplex c) (convertExpr e)
    where convertComplex :: Abs.Complex -> Complex Double
          convertComplex (Abs.CComp (Abs.Scalar r) (Abs.Scalar i)) = read (r ++ ":+" ++ i)
convertExpr (Abs.EApp f x) = convertExpr f `App` convertExpr x
convertExpr (Abs.EPlus a b) = convertExpr a `Plus` convertExpr b
convertExpr (Abs.EIfq c t f) = Ifq (convertExpr c) (convertExpr t) (convertExpr f)  
convertExpr (Abs.EIf c t f) = If (convertExpr c) (convertExpr t) (convertExpr f)  
convertExpr (Abs.ELet ls x) = Let (map (\(Abs.LLet p e) -> LLet (convertPattern p) (convertExpr e)) ls) (convertExpr x) 
    where convertPattern :: Abs.Pattern -> Pattern
          convertPattern (Abs.PVar (Abs.Ident s)) = PVar s
          convertPattern (Abs.PTup (Abs.Ident a) (Abs.Ident b)) = PTup a b 

convertType :: Abs.Type -> Type
convertType Abs.TQubit = TypeQubit
convertType Abs.TUnit  = TypeUnit
convertType (Abs.TTens a b) = convertType a :* convertType b 