{-# LANGUAGE DeriveFunctor, DeriveFoldable,  DeriveTraversable #-}
module Language.Verilog.AST where

import Control.Applicative

import Data.Foldable
import Data.Traversable

data Module iden modItem
  = Module iden [iden] [modItem]
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

data ModuleItem iden expr
  = Comment    String
  | Parameter  (Maybe (Range expr)) iden expr
  | Localparam (Maybe (Range expr)) iden expr
  | Input      (Maybe (Range expr)) [iden]
  | Output     (Maybe (Range expr)) [iden]
  | Inout      (Maybe (Range expr)) [iden]
  | Wire       (Maybe (Range expr)) [(iden, Maybe expr)]
  | Reg        (Maybe (Range expr)) [(iden, Maybe (Range expr))]
  | Integer    [iden]
  | Initial    (Stmt iden expr)
  | Always     (Sense iden expr) (Stmt iden expr)
  | Assign     (LValue iden expr) expr
  | Instance   iden [PortBinding iden expr] iden [PortBinding iden expr]
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

type PortBinding iden expr = (iden, Maybe expr)

data Expr iden expr
  = String     String
  | Literal    (Maybe Int) Literal
  | ExprCall   (Call iden expr)
  | UniOp      UniOp expr
  | BinOp      BinOp expr expr
  | Mux        expr expr expr
  | Repeat     expr [expr]
  | LValue     (LValue iden expr)
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

data LValue iden expr
  = Identifier iden
  | Concat     [expr] -- Shouldn't be expr, but that is what the standards stupidly say
  | Bit        iden expr
  | Range      iden (Range expr)
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

data Literal
  = Number Integer
  | HighImpedence
  | Undefined
  deriving (Eq, Show, Read)

data UniOp
  = Not
  | BWNot
  | UAdd
  | USub
  deriving (Eq, Show, Read)

data BinOp
  = And
  | Or
  | BWAnd
  | BWXor
  | BWOr
  | Mul
  | Div
  | Mod
  | Add
  | Sub
  | ShiftL
  | ShiftR
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  deriving (Eq, Show, Read)

data Stmt iden expr
  = Block                 (Maybe iden) [Stmt iden expr]
  | StmtReg               (Maybe (Range expr)) [(iden, Maybe (Range expr))]
  | StmtInteger           [iden]
  | Case                  expr [Case iden expr] (Maybe (Stmt iden expr))
  | BlockingAssignment    (LValue iden expr) expr
  | NonBlockingAssignment (LValue iden expr) expr
  | For                   (iden, expr) expr (iden, expr) (Stmt iden expr)
  | If                    expr (Stmt iden expr) (Stmt iden expr)
  | StmtCall              (Call iden expr)
  | Delay                 expr (Stmt iden expr)
  | Null
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

type Case iden expr = ([expr], (Stmt iden expr))

data Call iden expr
  = Call iden [expr]
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

data Sense iden expr
  = Sense        (LValue iden expr)
  | SenseOr      (Sense iden expr) (Sense iden expr)
  | SensePosedge (LValue iden expr)
  | SenseNegedge (LValue iden expr)
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

type Range expr = (expr, expr)
