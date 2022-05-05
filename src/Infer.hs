{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The main type system module --- contains type inference, type constraint generation and solving.
module Infer
    ( infer
    , InferError(..)
    ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.List                      ( delete
                                                , find
                                                )
import qualified Data.List.NonEmpty            as NE
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.Traversable               ( for )

import           Prettyprinter                  ( (<+>)
                                                , Pretty(..)
                                                )
import qualified Prettyprinter                 as PP

import qualified Assumptions                   as A
import           Capability
import           Control.Monad.Fresh
import           Syntax
import           Type

-- | The types of builtin operations.
--
-- Requires a set of all capabilities because of user-created effects.
-- (Some of the builtin operations are explicitly unsafe with respect to some capability
-- so we cannot know statically for which expression it is actually safe.)
builtinSchemes :: Capability -> M.Map Variable (Capability, Scheme)
builtinSchemes allCap = M.fromList
    [ ("#add", (allCap, Forall [] $ TInt :-> TInt :-> TInt))
    , ("#sub", (allCap, Forall [] $ TInt :-> TInt :-> TInt))
    , ("#mul", (allCap, Forall [] $ TInt :-> TInt :-> TInt))
    , ("#div", (allCap, Forall [] $ TInt :-> TInt :-> TInt))
    , ( "#cons"
      , ( allCap
        , Forall ["a"] $ TVar "a" :-> TList (TVar "a") :-> TList (TVar "a")
        )
      )
    , ( "#ref"
      , ( capComplement (singletonCap "ref")
        , Forall ["a"] (TVar "a" :-> TRef (TVar "a"))
        )
      )
    , ( "#assign"
      , (allCap, Forall ["a"] $ TRef (TVar "a") :-> TVar "a" :-> TUnit)
      )
    , ("#deref", (allCap, Forall ["a"] $ TRef (TVar "a") :-> TVar "a"))
    , ( "#exit"
      , (capComplement (singletonCap "exit"), Forall [] (TInt :-> TUnit))
      )
    ]
    where capComplement = capDifference allCap

-- | Returns the type 'Scheme' of the given 'Literal'
literalType :: Literal -> Scheme
literalType (BoolLiteral _) = Forall [] TBool
literalType (IntLiteral  _) = Forall [] TInt
literalType (TextLiteral _) = Forall [] TText
literalType UnitLiteral     = Forall [] TUnit
literalType NilLiteral      = Forall ["a"] $ TList (TVar "a")

-- | Represents the errors that can happen during type inference.
data InferError
    = UnificationFail Type Type
    | InfiniteType TV Type
    | UnboundVariables [Variable] [Variable]
    | UnsafeVariables [Variable] [(Variable, BaseCapability)]
    deriving stock (Eq, Ord, Show)

instance Pretty InferError where
    pretty (UnificationFail t1 t2) = PP.vsep
        [ "Cannot unify type"
        , PP.indent 4 (pretty t1)
        , "with type"
        , PP.indent 4 (pretty t2)
        ]
    pretty (InfiniteType x t) =
        PP.vsep ["Infinite type", PP.indent 4 (PP.pretty (CEq (TVar x) t))]
    pretty (UnboundVariables defNames vars) = PP.vsep
        (   "Unbound variables during type inference for:"
        <+> PP.hsep (PP.punctuate PP.comma (PP.squotes . pretty <$> defNames))
        :   map (PP.indent 4 . PP.align . prettyVariableWithLoc) vars
        )
    pretty (UnsafeVariables defNames unsafe) = PP.vsep
        (   "Unsafe variables during type inference for:"
        <+> PP.hsep (PP.punctuate PP.comma (PP.squotes . pretty <$> defNames))
        :   concatMap
                (\(var, cap) ->
                    [ PP.indent 4 (PP.align $ prettyVariableWithLoc var)
                    , "but the definition does not possess the capability:"
                    , PP.indent 4 (PP.squotes (pretty cap))
                    ]
                )
                unsafe
        )

-- | Set of monomorphic variables @M@ in the thesis.
type InferMonos = S.Set TV

-- | Inference environment contains the current monomorphic variables
-- together with all of the possible capabilities.
data InferEnv = InferEnv
    { inferMonos   :: InferMonos
    , inferAllCaps :: Capability
    }

makeLensesFor [("inferMonos", "_inferMonos"), ("inferAllCaps", "_inferAllCaps")] ''InferEnv

-- | The infer monad allows to: read from the 'InferEnv', create fresh variables and throw 'InferError'.
newtype Infer a = Infer { unInfer :: ReaderT InferEnv (FreshT TV (Except InferError)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader InferEnv, MonadFresh TV, MonadError InferError)

-- | Combinator that runs a computation which has the input type variable
-- marked as monomorphic.
withMono :: TV -> Infer a -> Infer a
withMono m = local (\r -> r & _inferMonos %~ S.insert m)

-- | 'withMono' for multiple type variables.
withMonos :: InferMonos -> Infer a -> Infer a
withMonos m = local (\r -> r & _inferMonos %~ flip S.union m)

-- | Helper function to actually run the 'Infer' monad with sensible defaults.
runInfer :: Capability -> Infer a -> Either InferError a
runInfer allCaps m =
    m
        & unInfer
        & flip runReaderT (InferEnv S.empty allCaps)
        & flip evalFreshT (initialFreshState "t" id)
        & runExcept

-- | This function takes an expression and returns:
-- * generated assumptions
-- * generated constraints
-- * generated type
-- * the expression with types substituted in 'Binding's
--
-- This corresponds to rule set 9 (page 43) in the thesis.
inferExpr :: Expr () -> Infer (A.Assumptions, [Constraint], Type, Expr Type)
inferExpr (Literal lit) = do
    t <- instantiate (literalType lit)
    pure (mempty, [], t, Literal lit)
inferExpr (Var x) = do
    tv <- TVar <$> fresh
    pure (A.singleton x tv, [], tv, Var x)
inferExpr (App e1 e2) = do
    (as1, cs1, t1, e1') <- inferExpr e1
    (as2, cs2, t2, e2') <- inferExpr e2
    tv                  <- TVar <$> fresh
    let cs' = [CEq t1 (t2 :-> tv)] <> cs1 <> cs2
    pure (as1 <> as2, cs', tv, App e1' e2')
inferExpr (Lam (Binding x e ())) = do
    m <- fresh
    let tv = TVar m

    (as, cs, t, e') <- withMono m (inferExpr e)

    let as' = as `A.removeType` x
    let cs' = [ CEq t' tv | t' <- x `A.lookupType` as ] <> cs

    pure (as', cs', tv :-> t, Lam $ Binding x e' tv)
inferExpr (Let (Binding x e1 ()) e2) = do
    (as1, cs1, t1, e1') <- inferExpr e1
    (as2, cs2, t2, e2') <- inferExpr e2
    monos               <- asks inferMonos

    let as' = as1 <> as2 `A.removeType` x
    let cs' =
            cs1 <> cs2 <> [ CImpInst t' monos t1 | t' <- x `A.lookupType` as2 ]

    pure (as', cs', t2, Let (Binding x e1' t1) e2')
inferExpr (Match e cases) = do
    (as1, cs1, t1, e') <- inferExpr e

    inferred           <- traverse (inferCase t1) cases
    let (as2, cs2) = foldMap (\(a, c, _, _) -> (a, c)) inferred
    ret <- TVar <$> fresh
    let cs3 = map (\(_, _, branchRet, _) -> CEq ret branchRet) inferred
    let cases' = zipWith
            (\(_, _, _, inferredExpr) (Case p _) -> Case p inferredExpr)
            inferred
            cases

    pure (as1 <> as2, cs1 <> cs2 <> cs3, ret, Match e' cases')
inferExpr (Outta cap x e1 e2) = do
    allCaps <- asks inferAllCaps
    let cap' | cap == noCap = allCaps
             | otherwise    = cap

    (as1, cs1, t1, e1') <- inferExpr e1
    (as2, cs2, t2, e2') <- inferExpr e2

    let as' = as1 <> as2 `A.removeWrt` (x, cap')
    tv <- TVar <$> fresh
    let cs' =
            cs1
                <> cs2
                <> [CEq t1 (TSafe cap' tv)]
                <> [ CEq t' tv | t' <- x `A.lookupType` as2 ]

    pure (as', cs', t2, Outta cap' x e1' e2')
inferExpr (Into cap e) = do
    allCaps <- asks inferAllCaps
    let cap' | cap == noCap = allCaps
             | otherwise    = cap

    (as, cs, t, e') <- inferExpr e
    let as' = as `A.makeSafeWrt` cap'
    pure (as', cs, TSafe cap' t, Into cap' e')

-- | Generates constraints for patterns.
-- This is not in the thesis, but it's mostly straightforward
-- (it suffices to follow Typing Haskell in Haskell by Mark P. Jones as a reference)
inferPattern
    :: Type
    -> Pattern Variable
    -> Infer ([Constraint], A.Assumptions, InferMonos, [Variable])
inferPattern matchedType = \case
    PatternBlank      -> pure ([], mempty, mempty, [])
    PatternVariable x -> do
        tv <- fresh
        let t = TVar tv
        pure ([CEq matchedType t], A.singleton x t, S.singleton tv, [])
    PatternLiteral lit -> do
        litType <- instantiate (literalType lit)
        pure ([CEq matchedType litType], mempty, mempty, [])
    PatternSafe cap p -> do
        allCaps <- asks inferAllCaps
        let cap' | cap == noCap = allCaps
                 | otherwise    = cap

        tv                    <- TVar <$> fresh
        (cs, as, monos, safe) <- inferPattern tv p
        let cs' = CEq matchedType (TSafe cap' tv) : cs
        pure (cs', as, monos, safe <> A.keys as)
    PatternCons pHead pTail -> do
        tv1                       <- TVar <$> fresh
        tv2                       <- TVar <$> fresh
        (cs1, as1, monos1, safe1) <- inferPattern tv1 pHead
        (cs2, as2, monos2, safe2) <- inferPattern tv2 pTail

        let csCons = [CEq tv2 (TList tv1), CEq matchedType tv2]
        let cs'    = cs1 <> cs2 <> csCons
        let as'    = as1 <> as2
        let monos' = monos1 <> monos2
        let safe'  = safe1 <> safe2
        pure (cs', as', monos', safe')

-- | Infer a single case.
inferCase
    :: Type
    -> Case (Pattern Variable) (Expr ())
    -> Infer (A.Assumptions, [Constraint], Type, Expr Type)
inferCase matchedType (Case pat e) = do
    tv                            <- TVar <$> fresh
    (cs1, as1, monos     , safes) <- inferPattern tv pat
    (as2, cs2, resultType, e'   ) <- withMonos monos $ inferExpr e

    let as'  = as2 `A.removeTypeMany` A.keys as1
    let as'' = as' `A.removeMany` safes
    let monoCs = foldMap
            (\(x, t) -> [ CEq t t' | t' <- x `A.lookupType` as2 ])
            (A.typeAssumptions as1)
    let cs' = CEq tv matchedType : cs1 <> cs2 <> monoCs

    pure (as'', cs', resultType, e')

-- | Infers a declaration.
-- Corresponds to algorithm 'Infer' in the thesis.
inferDecl
    :: M.Map Variable (Capability, Scheme)
    -> Definition ()
    -> Infer (Definition Scheme)
inferDecl schemes (Define defs) = do
    let names      = defs ^.. traverse . _boundVar
    let nameSet    = S.fromList names
    let exprs      = defs ^.. traverse . _boundExpr

    let bindingSet = nameSet `S.union` M.keysSet schemes
    inferred <- traverse inferExpr exprs

    let assumptions = foldMap (\(as, _, _, _) -> as) inferred
    let constraints = foldMap (\(_, cs, _, _) -> cs) inferred
    let referentialConstraints = foldMap
            (\((as, _, t, _), name) ->
                [ CEq t' t | t' <- name `A.lookupType` as ] -- monos are mempty here!
            )
            (zip inferred names)

    let typedDefs = NE.fromList $ zipWith
            (\x (t', e') -> Binding x e' t')
            names
            (map (\(_, _, t, e) -> (t, e)) inferred)

    let unbound = A.keysSet (assumptions `A.removeMany` S.toList bindingSet)
    unless (S.null unbound) $ do
        throwError $ UnboundVariables names (S.toList unbound)

    let safeSchemes = [ (x, cap) | (x, (cap, _)) <- M.toList schemes ]
    let unsafe      = A.safeSet (assumptions `A.removeManyWrt` safeSchemes)
    unless (S.null unsafe) $ do
        throwError $ UnsafeVariables names (S.toList unsafe)

    let instantiations =
            [ CExpInst t s
            | (x, (_, s)) <- M.toList schemes
            , t           <- x `A.lookupType` assumptions
            ]
    sub <- solve (constraints <> referentialConstraints <> instantiations)

    let result = runTyping (assignTypeDefn $ Define typedDefs) sub
    pure result

-- | Top-level type inference function.
infer
    :: [EffectDefinition]
    -> [Definition ()]
    -> Either InferError [Definition Scheme]
infer effs = runInfer allCapabilities . go initialEnv
  where
    typeMap :: Definition Scheme -> M.Map Variable (Capability, Scheme)
    typeMap (Define defs) =
        let extractSafe scheme = case scheme of
                Forall as (TSafe cap t) -> (cap, Forall as t)
                _                       -> (noCap, scheme)
        in  M.fromList
                $   (\(Binding x _ s) -> (x, extractSafe s))
                <$> NE.toList defs

    go
        :: M.Map Variable (Capability, Scheme)
        -> [Definition ()]
        -> Infer [Definition Scheme]
    go _       []           = pure []
    go schemes (def : defs) = do
        def' <- inferDecl schemes def
        let schemes' = schemes `M.union` typeMap def'
        defs' <- go schemes' defs
        pure (def' : defs')

    initialEnv :: M.Map Variable (Capability, Scheme)
    initialEnv = builtinSchemes allCapabilities <> M.fromList
        (do -- uses the list monad since it's too large for a list comprehension
            Effect capName arms <- effs
            (n, t)              <- arms
            let cap    = singletonCap (capName ^. _varText)
            let safety = capDifference allCapabilities cap
            let s      = generalize mempty t
            pure (n, (safety, s))
        )

    allCapabilities :: Capability
    allCapabilities =
        Capability
            $         S.fromList
                          (map (\(Effect n _) -> BaseCapability $ n ^. _varText) effs)
            `S.union` S.map BaseCapability reservedCapNames

-- | Helper function which instantiates a type scheme (polytype)
-- with fresh type variables.
instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- traverse (const fresh) as
    let sub = Subst $ M.fromList $ zip as (TVar <$> as')
    pure $ apply sub t

-- | Helper function which generalizes a type.
generalize :: InferMonos -> Type -> Scheme
generalize free t = Forall as t
    where as = S.toList (ftv t `S.difference` free)

-- | Environment containing the satisfying substitution and the current bound type variables.
-- Used for populating the AST with polytypes.
data TypeEnv = TypeEnv
    { typeVars :: InferMonos
    , typeSub  :: Subst
    }
    deriving stock (Eq, Ord, Show)

-- | Monad which can read from 'TypeEnv'.
type Typing a = Reader TypeEnv a

-- | Runs the 'Typing' monad with an initial substitution.
runTyping :: Typing a -> Subst -> a
runTyping typing sub = runReader typing (TypeEnv mempty sub)

-- | Generalizes a type according to the information in 'Typing'.
-- Also applies the satisfying type substitution.
generalizeTyping :: Type -> Typing Scheme
generalizeTyping t = do
    TypeEnv { typeVars, typeSub } <- ask
    pure $ generalize typeVars $ apply typeSub t

-- | Locally adds a list of active type variables into the 'Typing' context.
withVars :: [TV] -> Typing a -> Typing a
withVars vars =
    local $ \env -> env { typeVars = typeVars env `S.union` S.fromList vars }

-- | Takes an expression which has 'Type' (monotypes) in its bindings
-- and returns an expression which has 'Scheme' (polytypes) in its bindings.
assignTypeExpr :: Expr Type -> Typing (Expr Scheme)
assignTypeExpr (Literal lit        ) = pure $ Literal lit
assignTypeExpr (Var     x          ) = pure $ Var x
assignTypeExpr (App e1 e2) = App <$> assignTypeExpr e1 <*> assignTypeExpr e2
assignTypeExpr (Lam (Binding x e t)) = do
    s@(Forall as _) <- generalizeTyping t
    e'              <- withVars as $ assignTypeExpr e
    pure $ Lam $ Binding x e' s
assignTypeExpr (Let (Binding x e1 t) e2) = do
    s@(Forall as _) <- generalizeTyping t
    e1'             <- assignTypeExpr e1
    e2'             <- withVars as $ assignTypeExpr e2
    pure $ Let (Binding x e1' s) e2'
assignTypeExpr (Match matchExpr cases) = do
    matchExpr' <- assignTypeExpr matchExpr
    cases'     <- traverse (\(Case p e) -> Case p <$> assignTypeExpr e) cases
    pure $ Match matchExpr' cases'
assignTypeExpr (Outta cap x e1 e2) =
    Outta cap x <$> assignTypeExpr e1 <*> assignTypeExpr e2
assignTypeExpr (Into cap e) = Into cap <$> assignTypeExpr e

-- | Takes a definition which has monotypes in its bindings
-- and returns an expression which has polytypes in its bindings.
assignTypeDefn :: Definition Type -> Typing (Definition Scheme)
assignTypeDefn (Define bnds) = do
    bnds' <- for bnds $ \(Binding x e t) -> do
        s@(Forall as _) <- generalizeTyping t
        e'              <- withVars as $ assignTypeExpr e
        pure $ Binding x e' s
    pure $ Define bnds'

-- | Takes a set of constraints and produces a satisfying substitution.
--
-- Corresponds to algorithm 'Solve' in the thesis.
solve :: [Constraint] -> Infer Subst
solve [] = pure mempty
solve cs = solveWith (nextSolvable cs)
  where
    chooseOne xs = [ (x, x `delete` xs) | x <- xs ]

    nextSolvable xs = case find solvable (chooseOne xs) of
        Just c  -> c
        Nothing -> error "solve: nextSolvable: impossible!"

    solveWith :: (Constraint, [Constraint]) -> Infer Subst
    solveWith (CEq t1 t2, rest) = do
        sub1 <- unify t1 t2
        sub2 <- solve (sub1 `apply` rest)
        pure (sub2 <> sub1)
    solveWith (CImpInst t1 monos t2, rest) = do
        let s = generalize monos t2
        solve (CExpInst t1 s : rest)
    solveWith (CExpInst t s, rest) = do
        s' <- instantiate s
        solve (CEq t s' : rest)

-- | Takes two types and returns the most general unifier.
-- Corresponds to 'mgu' in the thesis.
unify :: Type -> Type -> Infer Subst
unify t1 t2 | t1 == t2        = pure mempty
unify (TVar tv)   t           = tv `bind` t
unify t           (TVar tv  ) = tv `bind` t
unify (t1 :-> t2) (t3 :-> t4) = do
    sub1 <- unify t1 t3
    sub2 <- unify (sub1 `apply` t2) (sub1 `apply` t4)
    pure (sub2 <> sub1)
unify (TList t1) (TList t2)                          = unify t1 t2
unify (TSafe cap1 t1) (TSafe cap2 t2) | cap1 == cap2 = unify t1 t2
unify (TRef t1) (TRef t2)                            = unify t1 t2
unify t1 t2 = throwError (UnificationFail t1 t2)

-- | Helper function for unifying a type with a type variable.
bind :: TV -> Type -> Infer Subst
bind tv t | t == TVar tv = pure mempty
          | occursCheck  = throwError (InfiniteType tv t)
          | otherwise    = pure $ Subst $ M.singleton tv t
    where occursCheck = tv `S.member` ftv t
