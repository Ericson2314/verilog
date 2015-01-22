module Language.Verilog.DSLInstances () where

import Data.Bits
import Data.List
import Data.Maybe
import Data.Monoid
import Text.Printf

import Data.BitVec
import Language.Verilog.AST

instance Show Module where
  show (Module name ports items) = unlines
    [ "module " ++ name ++ (if null ports then "" else "(" ++ commas ports ++ ")") ++ ";"
    , unlines' $ map show items
    , "endmodule"
    ]

instance Show ModuleItem where
  show a = case a of
    Comment    a     -> "// " ++ a
    Parameter  r n e -> printf "parameter %s%s = %s;"  (showRange r) n (showExprConst e)
    Localparam r n e -> printf "localparam %s%s = %s;" (showRange r) n (showExprConst e)
    Input      r a   -> printf "input  %s%s;" (showRange r) (commas a)
    Output     r a   -> printf "output %s%s;" (showRange r) (commas a)
    Inout      r a   -> printf "inout  %s%s;" (showRange r) (commas a)
    Wire       r a   -> printf "wire   %s%s;" (showRange r) (commas [ a ++ showAssign r | (a, r) <- a ])
    Reg        r a   -> printf "reg    %s%s;" (showRange r) (commas [ a ++ showRange  r | (a, r) <- a ])
    Integer      a   -> printf "integer %s;"  $ commas a
    Initial    a     -> printf "initial\n%s" $ indent $ show a
    Always     a b   -> printf "always @(%s)\n%s" (show a) $ indent $ show b
    Assign     a b   -> printf "assign %s = %s;" (show a) (show b)
    Instance   m params i ports
      | null params -> printf "%s %s %s;"     m                                  i (showPorts show ports)
      | otherwise   -> printf "%s #%s %s %s;" m (showPorts showExprConst params) i (showPorts show ports)
    where
    showPorts :: (Expr -> String) -> [(Identifier, Maybe Expr)] -> String
    showPorts s ports = printf "(%s)" $ commas [ printf ".%s(%s)" i (if isJust arg then s $ fromJust arg else "") | (i, arg) <- ports ]
    showAssign :: Maybe Expr -> String
    showAssign a = case a of
      Nothing -> ""
      Just a -> printf " = %s" $ show a

showRange :: Maybe Range -> String
showRange Nothing = ""
showRange (Just (h, l)) = printf "[%s:%s] " (showExprConst h) (showExprConst l)

indent :: String -> String
indent a = '\t' : f a
  where
  f [] = []
  f (a : rest)
    | a == '\n' = "\n\t" ++ f rest
    | otherwise = a : f rest

unlines' :: [String] -> String
unlines' = intercalate "\n"

instance Show UniOp where
  show a = case a of
    Not   -> "!"
    BWNot -> "~"
    UAdd  -> "+"
    USub  -> "-"

instance Show BinOp where
  show a = case a of
    And    -> "&&"
    Or     -> "||"
    BWAnd  -> "&"
    BWXor  -> "^"
    BWOr   -> "|"
    Mul    -> "*"
    Div    -> "/"
    Mod    -> "%"
    Add    -> "+"
    Sub    -> "-"
    ShiftL -> "<<"
    ShiftR -> ">>"
    Eq     -> "=="
    Ne     -> "!="
    Lt     -> "<"
    Le     -> "<="
    Gt     -> ">"
    Ge     -> ">="

showBitVecDefault :: BitVec -> String
showBitVecDefault a = printf "%d'h%x" (width a) (value a)

showBitVecConst :: BitVec -> String
showBitVecConst a = show $ value a

instance Show Expr where show = showExpr showBitVecDefault

showExprConst :: Expr -> String
showExprConst = showExpr showBitVecConst

showExpr :: (BitVec -> String) -> Expr -> String
showExpr bv a = case a of
  String     a     -> printf "\"%s\"" a
  Number     a     -> bv a
  ConstBool  a     -> printf "1'b%s" (if a then "1" else "0")
  ExprLHS    a     -> show a
  ExprCall   a     -> show a
  UniOp      a b   -> printf "(%s %s)" (show a) (s b)
  BinOp      a b c -> printf "(%s %s %s)" (s b) (show a) (s c)
  Mux        a b c -> printf "(%s ? %s : %s)" (s a) (s b) (s c)
  Bit        a b   -> printf "(%s [%d])" (s a) b
  Repeat     a b   -> printf "{%s {%s}}" (showExprConst a) (commas $ map s b)
  Concat     a     -> printf "{%s}" (commas $ map s a)
  where
  s = showExpr bv

instance Num Expr where
  (+) = BinOp Add
  (-) = BinOp Sub
  (*) = BinOp Mul
  negate = UniOp USub
  abs = undefined
  signum = undefined
  fromInteger = Number . fromInteger

instance Bits Expr where
  (.&.) = BinOp BWAnd
  (.|.) = BinOp BWOr
  xor   = BinOp BWXor
  complement = UniOp BWNot
  shift        = error "Not supported: shift"
  rotate       = error "Not supported: rotate"
  bitSize      = error "Not supported: bitSize"
  bitSizeMaybe = error "Not supported: bitSizeMaybe"
  isSigned _ = False
  testBit = undefined
  bit = undefined
  popCount = undefined

instance Monoid Expr where
  mempty      = 0
  mappend a b = mconcat [a, b]
  mconcat     = Concat

instance Show LHS where
  show a = case a of
    LHS        a        -> a
    LHSBit     a b      -> printf "%s[%s]"    a (showExprConst b)
    LHSRange   a (b, c) -> printf "%s[%s:%s]" a (showExprConst b) (showExprConst c)

commas :: [String] -> String
commas = intercalate ", "

instance Show Stmt where
  show a = case a of
    Block                 Nothing  b        -> printf "begin\n%s\nend" $ indent $ unlines' $ map show b
    Block                 (Just a) b        -> printf "begin : %s\n%s\nend" a $ indent $ unlines' $ map show b
    StmtReg               a b               -> printf "reg    %s%s;" (showRange a) (commas [ a ++ showRange  r | (a, r) <- b ])
    StmtInteger           a                 -> printf "integer %s;" $ commas a
    Case                  a b Nothing       -> printf "case (%s)\n%s\nendcase"                 (show a) (indent $ unlines' $ map showCase b)
    Case                  a b (Just c)      -> printf "case (%s)\n%s\n\tdefault:\n%s\nendcase" (show a) (indent $ unlines' $ map showCase b) (indent $ indent $ show c)
    BlockingAssignment    a b               -> printf "%s = %s;" (show a) (show b)
    NonBlockingAssignment a b               -> printf "%s <= %s;" (show a) (show b)
    For                   (a, b) c (d, e) f -> printf "for (%s = %s; %s; %s = %s)\n%s" a (show b) (show c) d (show e) $ indent $ show f
    If                    a b Null          -> printf "if (%s)\n%s"           (show a) (indent $ show b)
    If                    a b c             -> printf "if (%s)\n%s\nelse\n%s" (show a) (indent $ show b) (indent $ show c)
    StmtCall              a                 -> printf "%s;" (show a)
    Delay                 a b               -> printf "#%s %s" (show a) (show b)
    Null                                    -> ";"

showCase :: Case -> String
showCase (a, b) = printf "%s:\n%s" (commas $ map show a) (indent $ show b)

instance Show Call where
  show (Call a b) = printf "%s(%s)" a (commas $ map show b)

instance Show Sense where
  show a = case a of
    Sense        a -> show a
    SenseOr      a b -> printf "%s or %s" (show a) (show b)
    SensePosedge a   -> printf "posedge %s" (show a)
    SenseNegedge a   -> printf "negedge %s" (show a)
