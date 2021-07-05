module Frontend.Intermediate.AST
    ( -- * Types
      Type(..)
    , Pattern(..)
    , Let(LLet)
    , Expr(..)
    , Arg(..)
    , Toplevel(..)
    , Program
    ) where

import Data.Complex ( Complex )

type Program = [Toplevel]

data Toplevel
    = ToplF String [Arg] Expr 
    | ToplFT String [Arg] Type Expr
  deriving (Eq, Show, Read)

type Arg = [(String, Type)]

data Expr
    = Var String
    | QFalse
    | QTrue
    | Tup [Expr]
    | Mul (Complex Double) Expr
    | App Expr Expr
    | Plus Expr Expr
    | Ifq Expr Expr Expr
    | If Expr Expr Expr
    | Let [Let] Expr
  deriving (Eq, Show, Read)

data Let = LLet Pattern Expr
  deriving (Eq, Show, Read)

data Pattern = PVar String | PTup String String
  deriving (Eq, Ord, Show, Read)

data Type = TypeQubit | TypeUnit | Type :* Type
  deriving (Eq, Ord, Show, Read)
