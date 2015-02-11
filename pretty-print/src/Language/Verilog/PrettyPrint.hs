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


ppTuple :: (PrettyPrint x, PrettyPrint y) => (x, y) -> Doc
ppTuple (x, y) = pp x <+> pp y

-- TODO orphan instances
instance PrettyPrint Int where
  pp = showAdapt


instance (PrettyPrint iden, PrettyPrint modItem) => PrettyPrint (Module iden modItem) where
  pp (Module name ports items) = vsep [ hsep ["module", pp name, ports', ";"]
                                      , indent ind $ vsep $ pp <$> items
                                      , "endmodule"
                                      ]
    where ports' = if null ports
                   then ""
                   else tupled $ ppTuple <$> ports

instance PrettyPrint Direction where
  pp = \case
    Input  -> "input"
    Output -> "output"
    Inout  -> "inout"

instance PrettyPrint iden => PrettyPrint (Decl iden) where
  pp (Decl ty0 iden0) = help ty0 $ pp iden0
    where help ty iden = case ty of
            Wire  n             -> simple n "wire"
            Reg   n             -> simple n "reg"
            Logic n             -> simple n "logic"
            Nominal idenTy      -> pp idenTy <+> iden
            UnpackedArray ty' n -> help ty' $ iden <> brackets (pp n)
            where simple n t = t <+> pp (Range n 0) <+> iden

instance forall iden expr. (PrettyPrint iden, PrettyPrint expr) => PrettyPrint (ModuleItem iden expr) where
  pp = \case
    Comment    a     -> "// " <> text (pack a)
    LocalNet   decl  -> pp decl <> ";"
    Parameter  r n e -> fill 7 "parameter"  <+> ppRange r <> pp n <+> "=" <+> pp e <> ";"
    Localparam r n e -> fill 7 "localparam" <+> ppRange r <> pp n <+> "=" <+> pp e <> ";"
    --Input      r a   -> fill 7 "input"      <+> ppRange r <> commas (pp <$> a) <> ";"
    --Output     r a   -> fill 7 "output"     <+> ppRange r <> commas (pp <$> a) <> ";"
    --Inout      r a   -> fill 7 "inout"      <+> ppRange r <> commas (pp <$> a) <> ";"
    --Wire       r a   -> fill 7 "wire"       <+> ppRange r <> commas [ pp a <> (opt $ pp <$> e) | (a, e) <- a ] <> ";"
    --Reg        r a   -> fill 7 "reg"        <+> ppRange r <> commas [ pp a <> (ppRange r) | (a, r) <- a ] <> ";"
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

instance PrettyPrint iden => PrettyPrint (NominalType iden) where
  pp = \case
    Struct mname decls -> "struct packed" <+> (opt $ pp <$> mname) <%> decls' -- <> ";"
      where decls' = semiBraces $ pp <$> decls

    Enum mnane msize ctors -> "enum" <+> msize' <%> ctors'
      where msize' = opt $ ("bits"<>) <$> (\x -> pp $ Range x 0) <$> msize
            ctors' = encloseSep "{" "}" "," $ pp <$> ctors

    TypeDef nominal name -> "typedef" <+> pp nominal <%> pp name

instance (PrettyPrint iden, PrettyPrint expr) => PrettyPrint (Expr iden expr) where
  pp = \case
    LValue lvalue         -> pp lvalue
    Literal mSize lit     -> (opt $ (<> "'") <$> showAdapt <$> mSize) <> pp lit
    String str            -> showAdapt str

    StructLit mTName flds -> "'" <> (opt $ pp <$> mTName) <> flds'
      where flds' = encloseSep "{" "}" "," $ (\(f, e) -> pp f <> ":" <+> pp e) <$> flds

    ExprCall call         -> pp call
    UniOp op exp          -> parens $ pp op <+> pp exp
    BinOp left op right   -> sexp [pp left, pp op, pp right]
    Mux cond true false   -> sexp [pp cond, "?", pp true, ":", pp false]
    Repeat count es       -> braces $ pp count <+> encloseSep lbrace rbrace comma (pp <$> es)
    where sexp = encloseSep lparen rparen space

instance (PrettyPrint iden, PrettyPrint expr) => PrettyPrint (LValue iden expr) where
  pp = \case
    Identifier ident     -> pp ident
    Concat exprs         -> encloseSep lbrace rbrace comma $ pp <$> exprs
    Bit ident bit        -> pp ident <> (brackets $ pp bit)
    SubRange ident range -> pp ident <> pp range

instance PrettyPrint SizedLiteral where
  pp = \case
    Number n -> "d" <> showAdapt n
    HighImpedence -> "Z"
    Undefined     -> "X"

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

instance (PrettyPrint iden, PrettyPrint expr) => PrettyPrint (Stmt iden expr) where
  pp = \case
    Block mName stmts         -> vsep [ begin
                                      , indent ind $ vsep $ pp <$> stmts
                                      , "end"
                                      ]
      where begin = hsep $ ("begin" :) $ concat $ maybeToList $ (\x -> [":",x]) <$> pp <$> mName

    StmtReg a b               -> "reg    " <> ppRange a <> commas [ pp a <> ppRange r | (a, r) <- b ] <> ";"
    StmtInteger a             -> "integer" <+> commas (pp <$> a)
    Case a b defaultCase      -> vsep [ "case" <+> parens (pp a)
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



instance PrettyPrint x => PrettyPrint (Range x) where
  pp (Range hi lo) = brackets $ pp hi <> ":" <> pp lo

ppRange range = opt $ pp <$> range

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
