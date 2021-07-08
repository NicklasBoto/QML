module Type.Error where

import Frontend.Intermediate.AST ( Expr, Type )

data TypeError
    = TypeMismatch Type Type
    | NotOrthogonal Expr Expr
    | InnerProductNotDefined Expr Expr
    | VariableNotInScope String
    | UnexpectedError

instance Semigroup TypeError where
    e <> _ = e

instance Monoid TypeError where
    mempty = UnexpectedError

instance Show TypeError where
    show (TypeMismatch expected actual) =
        "Couldn't match expected type '" ++ show expected ++ "' with actual type '" ++ show actual

    show (NotOrthogonal t f) =
        "Quantum conditional branches must be orthogonal, ⟨" ++ show t ++ "|" ++ show f ++ "⟩ ≠ 0"

    show (InnerProductNotDefined u v) =
        "Inner product is not defined for given expressions, ⟨" ++ show u ++ "|" ++ show v ++ "⟩ : ?"

    show (VariableNotInScope v) =
        "Variable '" ++ v ++ "' not in scope"

    show UnexpectedError =
        "Something unexpected happened!"