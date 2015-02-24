{-# LANGUAGE OverloadedStrings #-}
module Idris.Core.Pretty (
  NameOutput (..),
  TextFormatting (..),
  OutputAnnotation (..),
  prettyEnv
  ) where

import Text.PrettyPrint.Annotated.Leijen
import Util.Pretty hiding (Str)
import qualified Data.Text as T
-- import Data.List (nub)

import Idris.Core.TT
-- import Idris.Core.CaseTree
import Idris.Core.ProofState
import Idris.Core.ProofTerm
import Idris.Core.SourcePos

-- -------------------------------------------------------------------------

instance Sized FC where
  size (FC f s e) = 4 + length f


-- -------------------------------------------------------------------------

  -- | Output annotation for pretty-printed name - decides colour
data NameOutput = TypeOutput | FunOutput | DataOutput | MetavarOutput | PostulateOutput

-- | Text formatting output
data TextFormatting = BoldText | ItalicText | UnderlineText

-- | Output annotations for pretty-printing
data OutputAnnotation = AnnName Name (Maybe NameOutput) (Maybe String) (Maybe String)
                        -- ^^ The name, classification, docs overview, and pretty-printed type
                      | AnnBoundName Name Bool
                        -- ^^ The name and whether it is implicit
                      | AnnConst Const
                      | AnnData String String -- ^ type, doc overview
                      | AnnType String String -- ^ name, doc overview
                      | AnnKeyword
                      | AnnFC FC
                      | AnnTextFmt TextFormatting
                      | AnnTerm [(Name, Bool)] (TT Name) -- ^ pprint bound vars, original term
                      | AnnSearchResult Ordering -- ^ more general, isomorphic, or more specific
                      | AnnErr Err

-- -------------------------------------------------------------------------

instance Sized ErrorReportPart where
  size (TextPart msg) = 1 + length msg
  size (TermPart t) = 1 + size t
  size (NamePart n) = 1 + size n
  size (SubReport rs) = 1 + size rs

instance Sized Err where
  size (Msg msg) = length msg
  size (InternalMsg msg) = length msg
  size (CantUnify _ left right err _ score) = size left + size right + size err
  size (InfiniteUnify _ right _) = size right
  size (CantConvert left right _) = size left + size right
  size (UnifyScope _ _ right _) = size right
  size (NoSuchVariable name) = size name
  size (NoTypeDecl name) = size name
  size (NotInjective l c r) = size l + size c + size r
  size (CantResolve trm) = size trm
  size (NoRewriting trm) = size trm
  size (CantResolveAlts _) = 1
  size (IncompleteTerm trm) = size trm
  size UniverseError = 1
  size ProgramLineComment = 1
  size (At fc err) = size fc + size err
  size (Elaborating _ n err) = size err
  size (ElaboratingArg _ _ _ err) = size err
  size (ProviderError msg) = length msg
  size (LoadingFailed fn e) = 1 + length fn + size e
  size _ = 1


-- -------------------------------------------------------------------------

instance Pretty Err OutputAnnotation where
  pretty (Msg m) = text m
  pretty (CantUnify _ l r e _ i) =
      text "Cannot unify" <+> colon <+> pretty l <+> text "and" <+> pretty r <+>
      nest nestingSize (text "where" <+> pretty e <+> text "with" <+> (text . show $ i))
  pretty (ProviderError msg) = text msg
  pretty err@(LoadingFailed _ _) = text (show err)
  pretty _ = text "Error"

instance (Pretty a OutputAnnotation) => Pretty (TC a) OutputAnnotation where
  pretty (OK ok) = pretty ok
  pretty (Error err) =
    text "Error" <+> colon <+> pretty err


-- -------------------------------------------------------------------------

instance Sized Name where
  size (UN n)     = 1
  size (NS n els) = 1 + length els
  size (MN i n) = 1
  size _ = 1

instance Pretty Name OutputAnnotation where
  pretty n@(UN n') = annotate (AnnName n Nothing Nothing Nothing) $ text (T.unpack n')
  pretty n@(NS un s) = annotate (AnnName n Nothing Nothing Nothing) . noAnnotate $ pretty un
  pretty n@(MN i s) = annotate (AnnName n Nothing Nothing Nothing) $
                      lbrace <+> text (T.unpack s) <+> (text . show $ i) <+> rbrace
  pretty n@(SN s) = annotate (AnnName n Nothing Nothing Nothing) $ text (show s)

instance Pretty [Name] OutputAnnotation where
  pretty = encloseSep empty empty comma . map pretty


-- -------------------------------------------------------------------------

instance Pretty ArithTy OutputAnnotation where
    pretty (ATInt ITNative) = text "Int"
    pretty (ATInt ITBig) = text "BigInt"
    pretty (ATInt ITChar) = text "Char"
    pretty (ATInt (ITFixed n)) = pretty n
    pretty (ATInt (ITVec e c)) = pretty e <> text "x" <> (text . show $ c)
    pretty ATFloat = text "Float"

instance Pretty NativeTy OutputAnnotation where
    pretty IT8  = text "Bits8"
    pretty IT16 = text "Bits16"
    pretty IT32 = text "Bits32"
    pretty IT64 = text "Bits64"

-- -------------------------------------------------------------------------

instance Sized Const where
  size _ = 1

instance Pretty Const OutputAnnotation where
  pretty (I i) = text . show $ i
  pretty (BI i) = text . show $ i
  pretty (Fl f) = text . show $ f
  pretty (Ch c) = text . show $ c
  pretty (Str s) = text s
  pretty (AType a) = pretty a
  pretty StrType = text "String"
  pretty TheWorld = text "%theWorld"
  pretty WorldType = text "prim__World"
  pretty BufferType = text "prim__UnsafeBuffer"
  pretty PtrType = text "Ptr"
  pretty ManagedPtrType = text "Ptr"
  pretty VoidType = text "Void"
  pretty Forgot = text "Forgot"
  pretty (B8 w) = text . show $ w
  pretty (B16 w) = text . show $ w
  pretty (B32 w) = text . show $ w
  pretty (B64 w) = text . show $ w


-- -------------------------------------------------------------------------

instance Sized Raw where
  size (Var name) = 1
  size (RBind name bind right) = 1 + size bind + size right
  size (RApp left right) = 1 + size left + size right
  size RType = 1
  size (RUType _) = 1
  size (RForce raw) = 1 + size raw
  size (RConstant const) = size const

instance Pretty Raw OutputAnnotation where
  pretty = text . show



-- -------------------------------------------------------------------------

instance Sized a => Sized (Binder a) where
  size (Lam ty) = 1 + size ty
  size (Pi _ ty _) = 1 + size ty
  size (Let ty val) = 1 + size ty + size val
  size (NLet ty val) = 1 + size ty + size val
  size (Hole ty) = 1 + size ty
  size (GHole _ ty) = 1 + size ty
  size (Guess ty val) = 1 + size ty + size val
  size (PVar ty) = 1 + size ty
  size (PVTy ty) = 1 + size ty


-- -------------------------------------------------------------------------

instance Sized UExp where
  size _ = 1


-- -------------------------------------------------------------------------

instance Sized NameType where
  size _ = 1

instance Pretty NameType OutputAnnotation where
  pretty = text . show


-- -------------------------------------------------------------------------


-- -------------------------------------------------------------------------

instance Sized a => Sized (TT a) where
  size (P name n trm) = 1 + size name + size n + size trm
  size (V v) = 1
  size (Bind nm binder bdy) = 1 + size nm + size binder + size bdy
  size (App l r) = 1 + size l + size r
  size (Constant c) = size c
  size Erased = 1
  size (TType u) = 1 + size u

instance Pretty a o => Pretty (TT a) o where
  pretty _ = text "test"



-- -------------------------------------------------------------------------

prettyEnv env t = prettyEnv' env t False
  where
    prettyEnv' env t dbg = prettySe 10 env t dbg

    bracket outer inner p
      | inner > outer = lparen <> p <> rparen
      | otherwise     = p

    prettySe p env (P nt n t) debug =
      pretty n <+>
        if debug then
          lbracket <+> pretty nt <+> colon <+> prettySe 10 env t debug <+> rbracket
        else
          empty
    prettySe p env (V i) debug
      | i < length env =
        if debug then
          text . show . fst $ env!!i
        else
          lbracket <+> text (show i) <+> rbracket
      | otherwise      = text "unbound" <+> text (show i) <+> text "!"
    prettySe p env (Bind n b@(Pi _ t _) sc) debug
      | noOccurrence n sc && not debug =
          bracket p 2 $ prettySb env n b debug <> prettySe 10 ((n, b):env) sc debug
    prettySe p env (Bind n b sc) debug =
      bracket p 2 $ prettySb env n b debug <> prettySe 10 ((n, b):env) sc debug
    prettySe p env (App f a) debug =
      bracket p 1 $ prettySe 1 env f debug <+> prettySe 0 env a debug
    prettySe p env (Proj x i) debug =
      prettySe 1 env x debug <+> text ("!" ++ show i)
    prettySe p env (Constant c) debug = pretty c
    prettySe p env Erased debug = text "[_]"
    prettySe p env (TType i) debug = text "Type" <+> (text . show $ i)

    -- Render a `Binder` and its name
    prettySb env n (Lam t) = prettyB env "λ" "=>" n t
    prettySb env n (Hole t) = prettyB env "?defer" "." n t
    prettySb env n (Pi _ t _) = prettyB env "(" ") ->" n t
    prettySb env n (PVar t) = prettyB env "pat" "." n t
    prettySb env n (PVTy t) = prettyB env "pty" "." n t
    prettySb env n (Let t v) = prettyBv env "let" "in" n t v
    prettySb env n (Guess t v) = prettyBv env "??" "in" n t v

    -- Use `op` and `sc` to delimit `n` (a binding name) and its type
    -- declaration
    -- e.g. "λx : Int =>" for the Lam case
    prettyB env op sc n t debug =
      text op <> pretty n <+> colon <+> prettySe 10 env t debug <> text sc

    -- Like `prettyB`, but handle the bindings that have values in addition
    -- to names and types
    prettyBv env op sc n t v debug =
      text op <> pretty n <+> colon <+> prettySe 10 env t debug <+> text "=" <+>
        prettySe 10 env v debug <> text sc

-- -------------------------------------------------------------------------



-- -------------------------------------------------------------------------


instance Pretty ProofState OutputAnnotation where
  pretty ps | [] <- holes ps =
    pretty (thname ps) <+> colon <+> text " no more goals."
  pretty ps | (h : hs) <- holes ps =
    let tm = pterm ps
        OK g = goal (Just h) tm 
        nm = thname ps in
    let wkEnv = premises g in
      text "Other goals" <+> colon <+> pretty hs <+>
      prettyPs wkEnv (reverse wkEnv) <+>
      text "---------- " <+> text "Focussing on" <> colon <+> pretty nm <+> text " ----------" <+>
      pretty h <+> colon <+> prettyGoal wkEnv (goalType g)
    where
      prettyGoal ps (Guess t v) =
        prettyEnv ps t <+> text "=?=" <+> prettyEnv ps v
      prettyGoal ps b = prettyEnv ps $ binderTy b

      prettyPs env [] = empty
      prettyPs env ((n, Let t v):bs) =
        nest nestingSize (pretty n <+> colon <+>
        prettyEnv env t <+> text "=" <+> prettyEnv env v <+>
        nest nestingSize (prettyPs env bs))
      prettyPs env ((n, b):bs) =
        nest nestingSize (pretty n <+> colon <+> prettyEnv env (binderTy b) <+>
        nest nestingSize (prettyPs env bs))


-- -------------------------------------------------------------------------
