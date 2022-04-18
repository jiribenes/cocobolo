{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Surface.Surface where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Pretty
import           Prettyprinter                  ( (<+>)
                                                , Pretty
                                                , pretty
                                                )
import qualified Prettyprinter                 as P

import           Capability
import           Surface.Loc
import           Syntax                         ( Case(..)
                                                , Literal(..)
                                                , Pattern(..)
                                                )

type Id = Loc Text
type TypeId = Loc Text

data BinOp = Add | Sub | Mul | Div | Cons | Assign
    deriving stock (Eq, Ord, Show, Generic)

data UnOp = Safe Capability | Deref
    deriving stock (Eq, Ord, Show, Generic)

data Safety = ExplicitSafe Capability | ImplicitUnsafe
    deriving stock (Eq, Ord, Show, Generic)

data Param = Param
    { paramSafety :: Safety
    , paramId     :: Id
    , paramType   :: Maybe Type
    }
    deriving stock (Eq, Ord, Show, Generic)

data Expr
    = Literal Literal
    | Identifier Id
    | Binary (Loc BinOp) Expr Expr
    | Unary (Loc UnOp) Expr
    | Call Expr [Expr]
    | Lam [Param] Expr
    | LetIn Safety Id (Maybe [Param]) Expr Expr
    | VarDecl Id Expr Expr Range -- has explicit range because of lowering
    | Match Expr [Case (Pattern Id) Expr]
    | Seq Expr Expr Range -- has explicit range because of lowering
    | Exit Expr Range     -- has explicit range because of lowering
    deriving stock (Eq, Ord, Show, Generic)

type EffectArm = (Id, Maybe [Param], Maybe Type)

data Decl
    = Let Safety Id (Maybe [Param]) (Maybe Type) Expr
    | Effect Id [EffectArm]
    deriving stock (Eq, Ord, Show, Generic)

data Type
    = TypeConstructor TypeId
    | TypeArrow Type Type
    | TypeVariable TypeId
    deriving stock (Eq, Ord, Show, Generic)

instance Pretty BinOp where
    pretty Add    = "+"
    pretty Sub    = "-"
    pretty Mul    = "*"
    pretty Div    = "/"
    pretty Cons   = "::"
    pretty Assign = ":="

instance Pretty UnOp where
    pretty (Safe cap) | cap == noCap = "safe"
                      | otherwise    = "safe<" <> pretty cap <> ">"
    pretty Deref          = "!"

instance Pretty Param where
    pretty (Param safety ident maybeType) =
        P.hsep $ pretty safety : pretty ident : ptype
      where
        ptype = case maybeType of
            Just typ -> [":" <+> pretty typ]
            Nothing  -> []

-- TODO: this double-spaces, fix it 
instance Pretty Safety where
    pretty (ExplicitSafe cap) | cap == noCap = "safe"
                              | otherwise    = "safe<" <> pretty cap <> ">"
    pretty ImplicitUnsafe = mempty

prettyBlock :: [P.Doc ann] -> Expr -> P.Doc ann
prettyBlock prevs (Seq e1 e2 _) = prettyBlock ((pretty e1 <> ";") : prevs) e2
prettyBlock prevs (VarDecl x e1 e2 _) =
    prettyBlock (P.hsep ["var", pretty x, ":=", pretty e1, "in"] : prevs) e2
prettyBlock prevs (LetIn safety x Nothing e1 e2) = prettyBlock
    (P.hsep ["let", pretty safety, pretty x, "=", pretty e1, "in"] : prevs)
    e2
prettyBlock prevs (LetIn safety x (Just params) e1 e2) = prettyBlock
    ( P.hsep
            [ "let"
            , pretty safety
            , pretty x <> P.tupled (pretty <$> params)
            , "="
            , pretty e1
            , "in"
            ]
    : prevs
    )
    e2
prettyBlock prevs e = P.align (P.vsep (reverse (pretty e : prevs)))

instance Pretty Expr where
    pretty (Literal    lit ) = pretty lit
    pretty (Identifier x   ) = pretty x
    pretty (Binary op e1 e2) = P.parens (pretty e1 <+> pretty op <+> pretty e2)
    pretty (Unary op e     ) = pretty op <+> pretty e
    pretty (Call  f  args  ) = pretty f <> P.tupled (pretty <$> args)
    pretty (Lam params e) =
        "fun" <> P.tupled (pretty <$> params) <+> "=>" <> withIndentPerhaps
            (pretty e)
    pretty l@LetIn{}   = prettyBlock [] l
    pretty v@VarDecl{} = prettyBlock [] v
    pretty s@Seq{}     = prettyBlock [] s
    pretty (Match e arms) =
        P.vsep ["match" <+> pretty e <+> "with", P.vcat (map pretty arms)]
    pretty (Exit e _) = "exit" <> P.parens (pretty e)

instance Pretty Type where
    pretty (TypeConstructor x) = pretty x
    pretty (TypeArrow t1@TypeArrow{} t2) =
        P.parens (pretty t1) <+> "->" <+> pretty t2
    pretty (TypeArrow t1 t2) = pretty t1 <+> "->" <+> pretty t2
    pretty (TypeVariable x ) = pretty x

prettyEffectArm :: EffectArm -> P.Doc ann
prettyEffectArm (name, maybeParams, maybeType) =
    "|" <+> pretty name <> params <> typ
  where
    params = case maybeParams of
        Just ps -> P.tupled (map pretty ps)
        Nothing -> mempty
    typ = case maybeType of
        Just ty -> ":" <+> pretty ty
        Nothing -> mempty

instance Pretty Decl where
    pretty (Let safety x maybeParams maybeType e) =
        "let"
            <+> pretty safety
            <+> pretty x
            <>  params
            <>  typ
            <+> "="
            <>  withIndentPerhaps (pretty e)
      where
        params = case maybeParams of
            Just ps -> P.tupled (map pretty ps)
            Nothing -> mempty
        typ = case maybeType of
            Just ty -> ":" <+> pretty ty
            Nothing -> mempty

    pretty (Effect name arms) =
        P.vsep
            [ "effect" <+> pretty name <+> "with"
            , P.vcat (map prettyEffectArm arms)
            ]
