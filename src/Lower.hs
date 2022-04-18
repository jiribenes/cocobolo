{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Lower where

import           Control.Lens                   ( (<&>)
                                                , (^.)
                                                )
import           Control.Monad.Except           ( MonadError(throwError)
                                                , runExcept
                                                )
import           Data.Bifunctor                 ( first )
import           Data.Either                    ( partitionEithers )
import           Data.Foldable                  ( foldl' )
import           Data.Function                  ( (&) )
import qualified Data.Graph                    as G
import qualified Data.List.NonEmpty            as NE
import           Prettyprinter                  ( Pretty(..) )

import           Capability                     ( reservedCapNames )
import           Control.Monad.Fresh
import qualified Data.Set                      as S
import qualified Prettyprinter                 as PP
import           Surface.Loc
import qualified Surface.Surface               as S
import           Syntax
import           Type

data LowerError
    = IncompleteTypeInfo S.EffectArm Variable
    | ExtraSafety S.EffectArm Variable
    | MissingReturnType S.EffectArm
    | UnknownTypeConstructor Variable
    | ReservedEffectName Variable

instance Pretty LowerError where
    pretty (IncompleteTypeInfo (armName, _, _) var) = PP.vsep
        [ "Incomplete type annotation"
        , PP.indent 4 (prettyVariableWithLoc var)
        , "in the definition of effectful function:"
        , PP.indent 4 (prettyVariableWithLoc (Variable armName))
        ]
    pretty (ExtraSafety (armName, _, _) var) = PP.vsep
        [ "Cannot make effectful parameter safe"
        , PP.indent 4 (prettyVariableWithLoc var)
        , "in the definition of effectful function:"
        , PP.indent 4 (prettyVariableWithLoc (Variable armName))
        ]
    pretty (MissingReturnType (armName, _, _)) = PP.vsep
        [ "Missing return type of effectful function"
        , PP.indent 4 (prettyVariableWithLoc (Variable armName))
        ]
    pretty (UnknownTypeConstructor var) =
        PP.vsep
            [ "Unknown type constructor"
            , PP.indent 4 (prettyVariableWithLoc var)
            ]
    pretty (ReservedEffectName var) = PP.vsep
        [ "Cannot redeclare predefined effect"
        , PP.indent 4 (prettyVariableWithLoc var)
        ]

-- Note: Variable identifiers cannot start with a @#@ sign,
-- therefore we can represent builtins simply as variables prepended with @#@.
lowerBinOp :: Loc S.BinOp -> Expr t -> Expr t -> Expr t
lowerBinOp (Loc op range) e1 = App (App (Var (LoweredVariable builtinVar)) e1)
  where
    builtinVar  = Loc builtinName range
    builtinName = case op of
        S.Add    -> "#add"
        S.Sub    -> "#sub"
        S.Mul    -> "#mul"
        S.Div    -> "#div"
        S.Cons   -> "#cons"
        S.Assign -> "#assign"

lowerUnOp :: Loc S.UnOp -> Expr t -> Expr t
lowerUnOp (Loc (S.Safe cap) _) e = Into cap e
lowerUnOp (Loc S.Deref range) e =
    App (Var (LoweredVariable (Loc "#deref" range))) e

lowerParam
    :: MonadFresh Variable m
    => S.Param
    -> m (Variable, UntypedExpr -> UntypedExpr)
lowerParam (S.Param S.ImplicitUnsafe ident _maybeType) =
    pure (Variable ident, id)
lowerParam (S.Param (S.ExplicitSafe cap) ident _maybeType) = do
    var <- fresh
    let prologue = Outta cap (Variable ident) (Var var)
    pure (var, prologue)

lowerExpr :: MonadFresh Variable m => S.Expr -> m UntypedExpr
lowerExpr (S.Literal    lit ) = pure $ Literal lit
lowerExpr (S.Identifier x   ) = pure $ Var $ Variable x
lowerExpr (S.Binary op e1 e2) = do
    e1' <- lowerExpr e1
    e2' <- lowerExpr e2
    let makeBuiltin = lowerBinOp op
    pure $ makeBuiltin e1' e2'
lowerExpr (S.Unary op e) = do
    e' <- lowerExpr e
    let makeBuiltin = lowerUnOp op
    pure $ makeBuiltin e'
lowerExpr (S.Call f []) = do
    f' <- lowerExpr f
    pure $ App f' (Literal UnitLiteral)
lowerExpr (S.Call f args) = do
    f'    <- lowerExpr f
    args' <- traverse lowerExpr args
    pure $ foldl' App f' args'
lowerExpr (S.Lam [] e) = do
    e'  <- lowerExpr e
    var <- fresh
    pure $ untypedLam var e'
lowerExpr (S.Lam params e) = do
    e'      <- lowerExpr e
    params' <- traverse lowerParam params
    let prologue = foldr (.) id (snd <$> params')
    pure $ foldr untypedLam (prologue e') (fst <$> params')
lowerExpr (S.LetIn S.ImplicitUnsafe x Nothing e1 e2) = do
    e1' <- lowerExpr e1
    e2' <- lowerExpr e2
    pure $ Let (Binding (Variable x) e1' ()) e2'
lowerExpr (S.LetIn (S.ExplicitSafe cap) x Nothing e1 e2) = do
    e1' <- lowerExpr e1
    e2' <- lowerExpr e2
    pure $ Into cap $ Let (Binding (Variable x) e1' ()) e2'
lowerExpr (S.LetIn s x (Just params) e1 e2) =
    lowerExpr (S.LetIn s x Nothing (S.Lam params e1) e2)
lowerExpr (S.VarDecl x e1 e2 range) = do
    e1' <- lowerExpr e1
    e2' <- lowerExpr e2
    let var = LoweredVariable $ Loc "#ref" range
    let ref = App (Var var) e1'
    pure $ Let (Binding (Variable x) ref ()) e2'
lowerExpr (S.Match expr cases) = do
    expr'  <- lowerExpr expr
    cases' <- traverse (lowerCase expr') cases
    pure $ Match expr' cases'
lowerExpr (S.Seq e1 e2 range) = do
    x <- fresh
    let var = LoweredVariable (Loc (x ^. _varText) range)
    e1' <- lowerExpr e1
    e2' <- lowerExpr e2
    pure $ App (untypedLam var e2') e1'
lowerExpr (S.Exit e range) = do
    e' <- lowerExpr e
    pure $ App (Var (LoweredVariable (Loc "#exit" range))) e'

untypedLam :: Variable -> Expr () -> Expr ()
untypedLam x e = Lam (Binding x e ())

lowerCase
    :: MonadFresh Variable m
    => UntypedExpr
    -> Case (Pattern S.Id) S.Expr
    -> m (Case (Pattern Variable) UntypedExpr)
lowerCase _matched (Case pat e) = do
    e' <- lowerExpr e
    pure (Case (Variable <$> pat) e')

lowerType :: MonadError LowerError m => S.Type -> m Type
lowerType (S.TypeConstructor locId) = case locThing locId of
    "Text" -> pure TText
    "Int"  -> pure TInt
    "Bool" -> pure TBool
    "Unit" -> pure TUnit
    _      -> throwError $ UnknownTypeConstructor (Variable locId)
lowerType (S.TypeArrow t1 t2) = do
    t1' <- lowerType t1
    t2' <- lowerType t2
    pure $ t1' :-> t2'
lowerType (S.TypeVariable locId) = pure $ TVar $ locThing locId

data SingleDefinition = SingleDefinition Variable UntypedExpr

lowerDecl
    :: (MonadFresh Variable m, MonadError LowerError m)
    => S.Decl
    -> m (Either SingleDefinition EffectDefinition)
lowerDecl (S.Let (S.ExplicitSafe cap) x Nothing _maybeType e) = do
    e' <- lowerExpr e
    pure $ Left $ SingleDefinition (Variable x) (Into cap e')
lowerDecl (S.Let S.ImplicitUnsafe x Nothing _maybeType e) = do
    e' <- lowerExpr e
    pure $ Left $ SingleDefinition (Variable x) e'
lowerDecl (S.Let s x (Just params) maybeType e) = do
    lowerDecl (S.Let s x Nothing maybeType (S.Lam params e))
lowerDecl (S.Effect x _) | locThing x `S.member` reservedCapNames =
    throwError $ ReservedEffectName (Variable x)
lowerDecl (S.Effect x arms) =
    Right . Effect (Variable x) <$> traverse lowerEffectArm arms

lowerEffectArm :: MonadError LowerError m => S.EffectArm -> m (Variable, Type)
lowerEffectArm arm@(name, maybeParams, maybeType) = do
    paramTypes <- case maybeParams of
        Just []     -> pure [TUnit]
        Just params -> traverse lowerParamType params
        Nothing     -> pure []
    resultType <- case maybeType of
        Just t  -> lowerType t
        Nothing -> throwError $ MissingReturnType arm

    let typ = foldr (:->) resultType paramTypes
    pure (Variable name, typ)
  where
    lowerParamType :: MonadError LowerError m => S.Param -> m Type
    lowerParamType (S.Param S.ImplicitUnsafe _ (Just t)) = lowerType t
    lowerParamType (S.Param S.ImplicitUnsafe n Nothing) =
        throwError $ IncompleteTypeInfo arm (Variable n)
    lowerParamType (S.Param S.ExplicitSafe{} n _) =
        throwError $ ExtraSafety arm (Variable n)

connectedComponents :: [SingleDefinition] -> [Definition ()]
connectedComponents =
    fmap sccToDefinition . G.stronglyConnCompR . fmap graphNode
  where
    graphNode (SingleDefinition x e) = (e, x, allVariables e)

    sccToDefinition
        :: G.SCC (UntypedExpr, Variable, [Variable]) -> Definition ()
    sccToDefinition = \case
        G.AcyclicSCC (e, x, _) -> Define $ Binding x e () NE.:| []
        G.CyclicSCC defs ->
            Define $ NE.fromList $ (\(e, x, _) -> Binding x e ()) <$> defs

lowerTopLevel
    :: [S.Decl] -> Either LowerError ([Definition ()], [EffectDefinition])
lowerTopLevel decls =
    decls
        &   traverse lowerDecl
        <&> partitionEithers
        &   flip evalFreshT (initialFreshState "_var_" FakeVariable)
        &   runExcept
        <&> first connectedComponents
