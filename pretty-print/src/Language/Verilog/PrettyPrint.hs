{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Verilog.PrettyPrint where

import           Control.Applicative

import           Data.Maybe (fromMaybe, maybeToList)
import           Data.Monoid
import           Data.Text.Lazy (pack)


import           Text.PrettyPrint.Leijen.Text hiding ((<>), (<$>), empty)
import qualified Text.PrettyPrint.Leijen.Text
import           Text.PrettyPrint.Leijen.Text.Class

import           Language.Verilog.AST

ind = 2

showAdapt :: Show s => s -> Doc
showAdapt = text . pack . show

opt :: Monoid d => Maybe d -> d
opt = fromMaybe mempty

(<%>) = (Text.PrettyPrint.Leijen.Text.<$>)

instance (PrettyPrint iden, PrettyPrint expr) => PrettyPrint (Module iden expr) where
  pp (Module name ports items) = vsep [ hsep ["module", pp name, ports', ";"]
                                      , indent ind $ vsep $ pp <$> items
                                      , "endmodule"
                                      ]
    where ports' = if null ports
                   then ""
                   else tupled $ pp <$> ports

instance forall iden expr. (PrettyPrint iden, PrettyPrint expr) => PrettyPrint (ModuleItem iden expr) where
  pp = \case
    Comment    a     -> "// " <> text (pack a)
    Parameter  r n e -> fill 7 "parameter"  <+> ppRange r <> pp n <+> "=" <+> pp e <> ";"
    Localparam r n e -> fill 7 "localparam" <+> ppRange r <> pp n <+> "=" <+> pp e <> ";"
    Input      r a   -> fill 7 "input"      <+> ppRange r <> commas (pp <$> a) <> ";"
    Output     r a   -> fill 7 "output"     <+> ppRange r <> commas (pp <$> a) <> ";"
    Inout      r a   -> fill 7 "inout"      <+> ppRange r <> commas (pp <$> a) <> ";"
    Wire       r a   -> fill 7 "wire"       <+> ppRange r <> commas [ pp a <> (opt $ pp <$> e) | (a, e) <- a ] <> ";"
    Reg        r a   -> fill 7 "reg"        <+> ppRange r <> commas [ pp a <> (ppRange r) | (a, r) <- a ] <> ";"
    Integer      a   -> fill 7 "integer"    <+> commas (pp <$> a) <> ";"
    Initial    a     -> "initial" <+> indent ind (pp a)
    Always     a b   -> "always" <+> "@" <> parens (pp a) <+> pp b
    Assign     a b   -> "assign" <> pp a <+> "=" <+> pp b <> ";"
    Instance   m params i ports -> case params of
      [] -> pp m <+> pp i <+> showPorts ports
      _  -> "%" <> pp m <+> "#%" <> showPorts params <+> pp i <+> showPorts ports
      where
        showPorts :: [(iden, Maybe expr)] -> Doc
        showPorts ports = tupled [ "." <> pp i <> (parens $ opt $ pp <$> arg) | (i, arg) <- ports ]

        showAssign :: Maybe expr -> Doc
        showAssign a = opt $ ("=" <+>) <$> pp <$> a

instance PrettyPrint UniOp where
  pp = \case
    Not   -> "!"
    BWNot -> "~"
    UAdd  -> "+"
    USub  -> "-"

instance PrettyPrint BinOp where
  pp = \case
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

instance (PrettyPrint iden, PrettyPrint expr) => PrettyPrint (Expr iden expr) where
  pp = \case
    String str            -> showAdapt str
    Literal mSize lit     -> (opt $ (<> "'") <$> showAdapt <$> mSize) <> pp lit
    ExprLHS lval          -> pp lval
    ExprCall call         -> pp call
    UniOp op exp          -> parens $ pp op <+> pp exp
    BinOp left op right   -> sexp [pp left, pp op, pp right]
    Mux cond true false   -> sexp [pp cond, "?", pp true, ":", pp false]
    Bit exp ind           -> parens $ pp exp <+> "[" <> showAdapt ind <> "]"
    Repeat count es       -> braces $ pp count <+> encloseSep lbrace rbrace comma (pp <$> es)
    Concat a              -> encloseSep lbrace rbrace comma  $ pp <$> a
    where sexp = encloseSep lparen rparen space

instance PrettyPrint Literal where
  pp = \case
    Number n -> showAdapt n
    HighImpedence -> "Z"
    Undefined     -> "X"

instance (PrettyPrint iden, PrettyPrint expr) => PrettyPrint (LHS iden expr) where
  pp = \case
    LHS        ident        -> pp ident
    LHSBit     exp bit      -> pp exp <> (brackets $ pp bit)
    LHSRange   exp (hi, lo) -> pp exp <> (brackets $ hsep [pp hi, ":", pp lo])

instance (PrettyPrint iden, PrettyPrint expr) => PrettyPrint (Stmt iden expr) where
  pp = \case
    Block mName stmts         -> vsep [ begin
                                      , indent ind $ vsep $ pp <$> stmts
                                      , "end"
                                      ]
      where begin = hsep $ ("begin" :) $ concat $ maybeToList $ (\x -> [":",x]) <$> pp <$> mName

    StmtReg a b               -> "reg    " <> ppRange a <> commas [ pp a <> ppRange r | (a, r) <- b ] <> ";"
    StmtInteger a             -> "integer" <+> commas (pp <$> a)
    Case a b defaultCase         -> vsep [ "case" <+> parens (pp a)
                                         , indent ind $ vsep $ (ppCase <$> b) ++ [defaultCase']
                                         , "endcase"
                                         ]
      where defaultCase' = opt $ ("default:" <+>) <$> pp <$> defaultCase

    BlockingAssignment    a b -> pp a <+> "=" <+> pp b <> ";"
    NonBlockingAssignment a b -> pp a <+> "<=" <+> pp b <> ";"
    For (a, b) c (d, e) f     -> "for" <> encloseSep lparen rparen semi [asgn a b, pp c, asgn d e]
                                 <%> indent ind (pp f)
      where asgn a b = pp a <+> "=" <+> pp b

    If a b c                  -> vsep [ "f" <+> parens (pp a)
                                      , indent ind (pp b)
                                      , "else"
                                      , pp c
                                      ]

    StmtCall call             -> pp call
    Delay a b                 -> "#" <> pp a <+> pp b

commas = encloseSep mempty mempty comma

ppRange :: PrettyPrint expr => Maybe (expr, expr) -> Doc
ppRange = \case
  Nothing     -> ""
  Just (h, l) -> brackets $ pp h <> pp l

ppCase :: (PrettyPrint iden, PrettyPrint expr) => Case iden expr -> Doc
ppCase (a, b) = commas (pp <$> a) <+> ":" <+> pp b

instance (PrettyPrint iden, PrettyPrint expr) => PrettyPrint (Call iden expr) where
  pp (Call fun args) = pp fun <> tupled (pp <$> args)

instance (PrettyPrint iden, PrettyPrint expr) => PrettyPrint (Sense iden expr) where
  pp = \case
    Sense        a   -> pp a
    SenseOr      a b -> sep [pp a, "or", pp b]
    SensePosedge a   -> hang ind $ sep ["posedge", pp a]
    SenseNegedge a   -> hang ind $ sep ["negedge", pp a]
