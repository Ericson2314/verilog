module Language.Verilog.AST
  ( Identifier
  , Module     (..)
  , ModuleItem (..)
  , Stmt       (..)
  , LHS        (..)
  , Expr       (..)
  , UniOp      (..)
  , BinOp      (..)
  , Sense      (..)
  , Call       (..)
  , PortBinding
  , Case
  , Range
  ) where

import Data.BitVec

type Identifier = String

data Module = Module Identifier [Identifier] [ModuleItem] deriving Eq

data ModuleItem
  = Comment    String
  | Parameter  (Maybe Range) Identifier Expr
  | Localparam (Maybe Range) Identifier Expr
  | Input      (Maybe Range) [Identifier]
  | Output     (Maybe Range) [Identifier]
  | Inout      (Maybe Range) [Identifier]
  | Wire       (Maybe Range) [(Identifier, Maybe Expr)]
  | Reg        (Maybe Range) [(Identifier, Maybe Range)]
  | Integer    [Identifier]
  | Initial    Stmt
  | Always     Sense Stmt
  | Assign     LHS Expr
  | Instance   Identifier [PortBinding] Identifier [PortBinding]
  deriving Eq

type PortBinding = (Identifier, Maybe Expr)

data Expr
  = String     String
  | Number     BitVec
  | ConstBool  Bool
  | ExprLHS    LHS
  | ExprCall   Call
  | UniOp      UniOp Expr
  | BinOp      BinOp Expr Expr
  | Mux        Expr Expr Expr
  | Bit        Expr Int
  | Repeat     Expr [Expr]
  | Concat     [Expr]
  deriving Eq

data UniOp = Not | BWNot | UAdd | USub deriving Eq

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
  deriving Eq

data LHS
  = LHS      Identifier
  | LHSBit   Identifier Expr
  | LHSRange Identifier Range
  deriving Eq

data Stmt
  = Block                 (Maybe Identifier) [Stmt]
  | StmtReg               (Maybe Range) [(Identifier, Maybe Range)]
  | StmtInteger           [Identifier]
  | Case                  Expr [Case] (Maybe Stmt)
  | BlockingAssignment    LHS Expr
  | NonBlockingAssignment LHS Expr
  | For                   (Identifier, Expr) Expr (Identifier, Expr) Stmt
  | If                    Expr Stmt Stmt
  | StmtCall              Call
  | Delay                 Expr Stmt
  | Null
  deriving Eq

type Case = ([Expr], Stmt)

data Call = Call Identifier [Expr] deriving Eq

data Sense
  = Sense        LHS
  | SenseOr      Sense Sense
  | SensePosedge LHS
  | SenseNegedge LHS
  deriving Eq

type Range = (Expr, Expr)
