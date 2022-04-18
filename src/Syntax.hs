{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

module Syntax
    (
    -- * Variable
      Variable(..)
    , _varText
    , prettyVariableWithLoc

     -- * Expressions
    , Expr(..)
    , UntypedExpr
    , allVariables
    , Pattern(..)
    , Case(..)
    , Literal(..)
    , builtinCons

     -- * Bindings
    , Binding(..)
    , _boundVar
    , _boundExpr
    , _boundType

     -- * Definitions
    , Definition(..)
    , EffectDefinition(..)
    ) where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Prettyprinter                  ( (<+>)
                                                , Pretty
                                                , pretty
                                                )
import qualified Prettyprinter                 as P

import           Control.Lens                   ( Lens'
                                                , Plated
                                                , (^.)
                                                , (^..)
                                                , cosmos
                                                , makePrisms
                                                , to
                                                )
import           Data.Data                      ( Data )
import           Data.List.NonEmpty             ( NonEmpty )
import qualified Data.List.NonEmpty            as NE
import           Data.String
import qualified Data.Text                     as T

import           Capability
import           Pretty
import           Surface.Loc
import           Type                           ( Type )

data Variable = Variable (Loc Text)
              | LoweredVariable (Loc Text)
              | FakeVariable Text
    deriving stock (Show, Data, Generic)

_varText :: Lens' Variable Text
_varText f var = setter <$> f getter
  where
    setter x = case var of
        Variable        locText -> Variable (x <$ locText)
        LoweredVariable locText -> LoweredVariable (x <$ locText)
        FakeVariable    text    -> FakeVariable text
    getter = case var of
        Variable        locText -> locThing locText
        LoweredVariable locText -> locThing locText
        FakeVariable    text    -> text

instance Eq Variable where
    x == y = x ^. _varText == y ^. _varText

instance Ord Variable where
    x <= y = x ^. _varText <= y ^. _varText

instance IsString Variable where
    fromString = FakeVariable . T.pack

instance Pretty Variable where
    pretty x = x ^. _varText . to pretty

prettyVariableWithLoc :: Variable -> P.Doc ann
prettyVariableWithLoc (Variable loc) =
    P.hsep [P.squotes (pretty (locThing loc)), "at:", pretty (locRange loc)]
prettyVariableWithLoc (LoweredVariable loc) = P.hsep
    [ P.squotes (pretty (locThing loc))
    , "at:"
    , pretty (locRange loc)
    , "(created from lowering)"
    ]
prettyVariableWithLoc (FakeVariable txt) =
    P.hsep [P.squotes (pretty txt), "(created automatically during lowering)"]


data Pattern var
    = PatternVariable { _patternVariable :: var }
    | PatternBlank
    | PatternLiteral { _patternLiteral :: Literal }
    | PatternSafe { _patternCapability :: Capability, _patternInner :: Pattern var }
    | PatternCons { _patternHead :: Pattern var, _patternTail :: Pattern var }
    deriving stock (Eq, Ord, Show, Data, Generic, Functor)

data Case pat a = Case
    { casePattern     :: pat
    , caseConsequence :: a
    }
    deriving stock (Eq, Ord, Show, Data, Generic, Functor, Foldable, Traversable)

data Literal
    = BoolLiteral Bool
    | IntLiteral Int
    | TextLiteral Text
    | UnitLiteral
    | NilLiteral
    deriving stock (Eq, Ord, Show, Data, Generic)

data Binding t = Binding
    { boundVar  :: Variable
    , boundExpr :: Expr t
    , boundType :: t
    }
    deriving stock (Eq, Ord, Show, Data, Generic, Functor, Foldable, Traversable)

_boundVar :: Lens' (Binding t) Variable
_boundVar f s = (\v -> s { boundVar = v }) <$> f (boundVar s)
_boundExpr :: Lens' (Binding t) (Expr t)
_boundExpr f s = (\e -> s { boundExpr = e }) <$> f (boundExpr s)
_boundType :: Lens' (Binding t) (Expr t)
_boundType f s = (\e -> s { boundExpr = e }) <$> f (boundExpr s)

data Expr t
    = Literal Literal
    | Var Variable
    | App (Expr t) (Expr t)
    | Lam (Binding t)
    | Let (Binding t) (Expr t)
    | Match (Expr t) [Case (Pattern Variable) (Expr t)]
    | Outta Capability Variable (Expr t) (Expr t)
    | Into Capability (Expr t)
    deriving stock (Eq, Ord, Show, Data, Generic, Functor, Foldable, Traversable)

type UntypedExpr = Expr ()

-- | Use default 'uniplate' implementation
instance Data t => Plated (Expr t) where

makePrisms ''Expr

allVariables :: Data t => Expr t -> [Variable]
allVariables e = e ^.. cosmos . _Var

newtype Definition t = Define (NonEmpty (Binding t))
    deriving stock (Eq, Ord, Show, Data, Generic, Functor, Foldable, Traversable)

data EffectDefinition = Effect Variable [(Variable, Type)]
    deriving stock (Eq, Ord, Show, Data, Generic)

instance Pretty EffectDefinition where
    pretty (Effect name arms) = P.vsep
        ( ("effect" <+> pretty name <+> "with")
        : map (\(n, t) -> "|" <+> pretty n <+> ":" <+> pretty t) arms
        )

instance Pretty Literal where
    pretty (IntLiteral  n    ) = pretty n
    pretty (TextLiteral txt  ) = P.dquotes $ pretty txt
    pretty (BoolLiteral True ) = "true"
    pretty (BoolLiteral False) = "false"
    pretty UnitLiteral         = "()"
    pretty NilLiteral          = "[]"

instance (Pretty pat, Pretty a) => Pretty (Case pat a) where
    pretty (Case pattern consequence) =
        "|" <+> pretty pattern <+> "=>" <> withIndentPerhaps
            (pretty consequence)

builtinCons :: Expr t -> Expr t -> Expr t
builtinCons eHead = App (App (Var "#cons") eHead)

prettyLam :: Pretty t => [(Variable, t)] -> Expr t -> P.Doc ann
prettyLam xs (Lam (Binding x e t)) = prettyLam ((x, t) : xs) e
prettyLam xs e =
    "\\" <> P.hsep (prettyNameType <$> reverse xs) <+> "=>" <> withIndentPerhaps
        (pretty e)
    where prettyNameType (x, t) = pretty x <+> ":" <+> pretty t

prettyBlock
    :: (Pretty t, Pretty (Binding t)) => [P.Doc ann] -> Expr t -> P.Doc ann
prettyBlock prevs (Let binding e2) =
    prettyBlock ((pretty binding <+> "in") : prevs) e2
prettyBlock prevs (Outta cap x e1 e2) = prettyBlock
    (("unbox<" <> pretty cap <> ">" <+> pretty x <+> "=" <+> pretty e1 <+> "in")
    : prevs
    )
    e2

prettyBlock prevs e = P.align (P.vsep (reverse (pretty e : prevs)))

instance Pretty t => Pretty (Expr t) where
    pretty (Literal lit    )      = pretty lit
    pretty (Var     x      )      = pretty x
    pretty (App e1 e2@App{})      = pretty e1 <+> P.parens (pretty e2)
    pretty (App e1 e2      )      = pretty e1 <+> pretty e2
    pretty l@Lam{}                = prettyLam [] l
    pretty l@Let{}                = prettyBlock [] l
    pretty o@Outta{}              = prettyBlock [] o
    pretty (Into cap e@Literal{}) = "box<" <> pretty cap <> ">" <+> pretty e
    pretty (Into cap e@Var{}    ) = "box<" <> pretty cap <> ">" <+> pretty e
    pretty (Into cap e) = "box<" <> pretty cap <> ">" <+> P.parens (pretty e)
    pretty (Match e cases) =
        "match" <+> pretty e <+> "with" <> P.hardline <> P.align
            (P.vsep (pretty <$> cases))

instance Pretty var => Pretty (Pattern var) where
    pretty (PatternVariable var) = pretty var
    pretty PatternBlank          = "_"
    pretty (PatternLiteral lit)  = pretty lit
    pretty (PatternSafe cap pat@PatternCons{}) =
        "safe<" <> pretty cap <> ">" <+> P.parens (pretty pat)
    pretty (PatternSafe cap pat) = "safe<" <> pretty cap <> ">" <+> pretty pat
    pretty (PatternCons headPat@PatternCons{} tailPat) =
        P.parens (pretty headPat) <+> "::" <+> pretty tailPat
    pretty (PatternCons headPat tailPat) =
        pretty headPat <+> "::" <+> pretty tailPat

instance {-# OVERLAPPING #-} Pretty (Binding ()) where
    pretty (Binding x e _) =
        "let" <+> pretty x <+> "=" <> withIndentPerhaps (pretty e)

instance Pretty t => Pretty (Binding t) where
    pretty (Binding x e t) = "let" <+> pretty x <> withIndentPerhaps
        (P.align $ P.sep [":" <+> pretty t, "=" <+> pretty e])

instance Pretty t => Pretty (Definition t) where
    pretty (Define bnds) = P.vsep $ NE.toList $ pretty <$> bnds
