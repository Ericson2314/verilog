{-# LANGUAGE DeriveFunctor, DeriveFoldable,  DeriveTraversable #-}
module Language.Verilog.AST where

import Control.Applicative

import Data.Foldable
import Data.Traversable

type Identifier = String

data Module expr
  = Module Identifier [Identifier] [ModuleItem expr]
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

data ModuleItem expr
  = Comment    String
  | Parameter  (Maybe (Range expr)) Identifier expr
  | Localparam (Maybe (Range expr)) Identifier expr
  | Input      (Maybe (Range expr)) [Identifier]
  | Output     (Maybe (Range expr)) [Identifier]
  | Inout      (Maybe (Range expr)) [Identifier]
  | Wire       (Maybe (Range expr)) [(Identifier, Maybe expr)]
  | Reg        (Maybe (Range expr)) [(Identifier, Maybe (Range expr))]
  | Integer    [Identifier]
  | Initial    (Stmt expr)
  | Always     (Sense expr) (Stmt expr)
  | Assign     (LHS expr) expr
  | Instance   Identifier [PortBinding expr] Identifier [PortBinding expr]
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

type PortBinding expr = (Identifier, Maybe expr)

data Expr expr
  = String     String
  | Literal    (Maybe Int) Literal
  | ConstBool  Bool
  | ExprLHS    (LHS expr)
  | ExprCall   (Call expr)
  | UniOp      UniOp expr
  | BinOp      BinOp expr expr
  | Mux        expr expr expr
  | Bit        expr Int
  | Repeat     expr [expr]
  | Concat     [expr]
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

data Literal
  = Number (Maybe Integer)
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

data LHS expr
  = LHS      Identifier
  | LHSBit   Identifier expr
  | LHSRange Identifier (Range expr)
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

data Stmt expr
  = Block                 (Maybe Identifier) [Stmt expr]
  | StmtReg               (Maybe (Range expr)) [(Identifier, Maybe (Range expr))]
  | StmtInteger           [Identifier]
  | Case                  expr [Case expr] (Maybe (Stmt expr))
  | BlockingAssignment    (LHS expr) expr
  | NonBlockingAssignment (LHS expr) expr
  | For                   (Identifier, expr) expr (Identifier, expr) (Stmt expr)
  | If                    expr (Stmt expr) (Stmt expr)
  | StmtCall              (Call expr)
  | Delay                 expr (Stmt expr)
  | Null
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

type Case expr = ([expr], (Stmt expr))

data Call expr
  = Call Identifier [expr]
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

data Sense expr
  = Sense        (LHS expr)
  | SenseOr      (Sense expr) (Sense expr)
  | SensePosedge (LHS expr)
  | SenseNegedge (LHS expr)
  deriving (Eq, Show, Read, Functor, Foldable, Traversable)

type Range expr = (expr, expr)
