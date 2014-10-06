{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Examples.Common where

import Data.Generics.Fixplate
import Data.Generics.Bifixplate
import Data.Bifunctor

data Expr e = LInt Integer
            | LBool Bool
            | Add e e
            | Eq e e
            | Not e
            deriving (Eq, Show, Functor, Foldable, Traversable)

instance EqF   Expr where equalF     = (==)
instance ShowF Expr where showsPrecF = showsPrec

data Stmt e s = If e s s
              | Block [s]
              | Assign e e
              | Print e
              deriving (Eq, Show, Functor, Foldable, Traversable)

instance Bifunctor Stmt where
  first f (If e s1 s2) = If (f e) s1 s2
  first _ (Block stmts) = Block stmts
  first f (Assign e1 e2) = Assign (f e1) (f e2)
  first f (Print e) = Print (f e)
  second = fmap

instance Eq e   => EqF   (Stmt e) where equalF     = (==)
instance Show e => ShowF (Stmt e) where showsPrecF = showsPrec

doubleExpr :: Mu Expr -> Mu Expr
doubleExpr = cata double
  where double (LInt n) = Fix $ LInt (n*2)
        double y = Fix y

doubleStmt :: Mu (Stmt (Mu Expr)) -> Mu (Stmt (Mu Expr))
doubleStmt = mapFirst doubleExpr

tenExpr :: Mu Expr
tenExpr = Fix (LInt 10)

printTen :: Mu (Stmt (Mu Expr))
printTen = Fix (Print tenExpr)

printTwenty :: Mu (Stmt (Mu Expr))
printTwenty = doubleStmt printTen
