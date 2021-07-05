module Type.Error where

import Frontend.Intermediate.AST ( Expr, Type )

data TypeError
    = TypeMismatch Type Type
    | NotOrthogonal Expr Expr
    | InnerProductNotDefined Expr Expr
    | VariableNotInScope String
    | UnexpectedError
    deriving Show

instance Semigroup TypeError where
    e <> _ = e

instance Monoid TypeError where
    mempty = UnexpectedError