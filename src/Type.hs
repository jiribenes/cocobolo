{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Type where
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import qualified Pretty
import           Prettyprinter                  ( (<+>)
                                                , Pretty(..)
                                                )
import qualified Prettyprinter                 as P

import           Capability
import Data.Data (Data)

type TV = Text
type TypeId = Text

infixr 2 :->

data Type
    = Type :-> Type
    | TVar TV
    | TList Type
    | TSafe Capability Type
    | TRef Type
    | TText
    | TInt
    | TBool
    | TUnit
    deriving stock (Eq, Ord, Show, Data, Generic)

instance Pretty Type where
    pretty (t1@(_ :-> _) :-> t2) = P.parens (pretty t1) <+> "->" <+> pretty t2
    pretty (t1           :-> t2) = pretty t1 <+> "->" <+> pretty t2
    pretty (TVar  x            ) = pretty x
    pretty (TRef  t@(_ :-> _)  ) = "^" <> P.parens (pretty t)
    pretty (TRef  t            ) = "^" <> pretty t
    pretty (TList t            ) = P.brackets (pretty t)
    pretty (TSafe cap t@(_ :-> _)) =
        P.angles (pretty cap) <+> P.parens (pretty t)
    pretty (TSafe cap t) = P.angles (pretty cap) <+> pretty t
    pretty TText         = "Text"
    pretty TInt          = "Int"
    pretty TBool         = "Bool"
    pretty TUnit         = "Unit"

data Scheme = Forall [TV] Type
    deriving stock (Eq, Ord, Show, Generic)

instance Pretty Scheme where
    pretty (Forall [] t) = pretty t
    pretty (Forall as t) =
        "forall"
            <+> P.align (P.hsep (pretty <$> as))
            <>  "."
            <>  Pretty.withIndentPerhaps (pretty t)

class FreeTypeVars a where
    ftv :: a -> S.Set TV

instance FreeTypeVars a => FreeTypeVars (S.Set a) where
    ftv = foldMap ftv

instance FreeTypeVars Type where
    ftv (t1 :-> t2) = ftv t1 `S.union` ftv t2
    ftv (TVar  x  ) = S.singleton x
    ftv (TList t  ) = ftv t
    ftv (TSafe _ t) = ftv t
    ftv (TRef t   ) = ftv t
    ftv _           = S.empty

instance FreeTypeVars TV where
    ftv = S.singleton

instance FreeTypeVars Scheme where
    ftv (Forall as t) = ftv t `S.difference` S.fromList as

closeOver :: Type -> Scheme
closeOver t = Forall (S.toList $ ftv t) t

refreshScheme :: Scheme -> Scheme
refreshScheme (Forall as t) = Forall bs (sub `apply` t)
  where
    sub = Subst $ M.fromList $ zip as (TVar . T.singleton <$> ['a' .. 'z'])
    bs  = sub `apply` as

data Constraint
    = CEq Type Type
    | CExpInst Type Scheme
    | CImpInst Type (S.Set TV) Type
  deriving stock (Eq, Ord, Show, Generic)

instance Pretty Constraint where
    pretty (CEq      x y) = pretty x <+> "~" <+> pretty y
    pretty (CExpInst t s) = pretty t <+> "≼" <+> pretty s
    pretty (CImpInst t1 mono t2) =
        pretty t1
            <+> "≼{"
            <>  P.concatWith (P.surround ", ") (pretty <$> S.toList mono)
            <>  "}"
            <+> pretty t2

solvable :: (Constraint, [Constraint]) -> Bool
solvable (CEq{}     , _) = True
solvable (CExpInst{}, _) = True
solvable (CImpInst _ monos t2, cs) =
    S.null $ (ftv t2 `S.difference` ftv monos) `S.intersection` atv cs

newtype Subst = Subst { unSubst :: M.Map TV Type }
  deriving stock (Eq, Ord, Show, Generic)

instance Pretty Subst where
    pretty (Subst m) = P.list (prettyOne <$> M.toList m)
        where prettyOne (tv, ty) = pretty tv <+> ":=" <+> pretty ty

instance Semigroup Subst where
    (Subst s1) <> (Subst s2) =
        Subst $ M.map (apply (Subst s1)) (s2 `M.union` s1)

instance Monoid Subst where
    mempty = Subst mempty

class Substitutable a where
    apply :: Subst -> a -> a

instance (Substitutable a, Ord a) => Substitutable (S.Set a) where
    apply = S.map . apply

instance Substitutable Type where
    apply sub       (   t1 :-> t2  ) = apply sub t1 :-> apply sub t2
    apply (Subst s) tv@(TVar  x    ) = M.findWithDefault tv x s
    apply sub       (   TList x    ) = TList (apply sub x)
    apply sub       (   TSafe cap x) = TSafe cap (apply sub x)
    apply sub       (   TRef x     ) = TRef (apply sub x)
    apply _         TText            = TText
    apply _         TInt             = TInt
    apply _         TBool            = TBool
    apply _         TUnit            = TUnit

instance Substitutable TV where
    apply (Subst s) x = case M.findWithDefault (TVar x) x s of
        TVar y -> y
        _      -> x

instance Substitutable Scheme where
    apply (Subst s) (Forall as t) = Forall as (apply s' t)
        where s' = Subst (foldr M.delete s as)

instance Substitutable Constraint where
    apply sub (CEq      t1 t2      ) = CEq (apply sub t1) (apply sub t2)
    apply sub (CExpInst t  s       ) = CExpInst (apply sub t) (apply sub s)
    apply sub (CImpInst t1 monos t2) = CImpInst (apply sub t1)
                                                monos'
                                                (apply sub t2)
        where
            -- https://stackoverflow.com/a/12864514
              monos' = ftv $ apply sub (S.map TVar monos)

instance Substitutable a => Substitutable [a] where
    apply = fmap . apply

class ActiveTypeVars a where
    atv :: a -> S.Set TV

instance ActiveTypeVars Constraint where
    atv (CEq      t1 t2) = ftv t1 `S.union` ftv t2
    atv (CExpInst t  s ) = ftv t `S.union` ftv s
    atv (CImpInst t1 monos t2) =
        ftv t1 `S.union` (ftv monos `S.intersection` ftv t2)

instance ActiveTypeVars a => ActiveTypeVars [a] where
    atv = foldMap atv
