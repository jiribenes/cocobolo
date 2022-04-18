{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module is designed to be imported in a qualified manner,
-- that is @import qualified Assumptions as A@.
module Assumptions
    ( Assumptions
    , typeAssumptions
    , singleton
    , extend
    , extendMany
    , makeSafeWrt
    , remove
    , removeMany
    , removeWrt
    , removeManyWrt
    , removeType
    , removeTypeMany
    , lookupType
    , keys
    , keysSet
    , safeSet
    ) where

import           Data.List                      ( nub
                                                , partition
                                                )
import           Data.Maybe                     ( mapMaybe )
import qualified Data.Set                      as S
import           GHC.Generics                   ( Generic )

import           Capability                     ( BaseCapability
                                                , Capability
                                                , capList
                                                , capMember
                                                )
import           Syntax                         ( Variable(..) )
import           Type                           ( Type )

data Assumption
    = HasType Variable Type
    | Safe Variable BaseCapability
  deriving stock (Show, Eq, Ord, Generic)

isSafeA :: Assumption -> Bool
isSafeA HasType{} = False
isSafeA Safe{}    = True

isSafeWrt :: Capability -> Assumption -> Bool
isSafeWrt cap (Safe _ baseCap) | not (baseCap `capMember` cap) = True
isSafeWrt _ _ = False

newtype Assumptions = Assumptions { unAssumptions :: [Assumption] }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Semigroup, Monoid)

typeAssumptions :: Assumptions -> [(Variable, Type)]
typeAssumptions (Assumptions a) = mapMaybe go a
  where
    go (HasType x t) = Just (x, t)
    go Safe{}        = Nothing

safe :: Assumptions -> [(Variable, BaseCapability)]
safe (Assumptions a) = mapMaybe go a
  where
    go (HasType _ _      ) = Nothing
    go (Safe    x baseCap) = Just (x, baseCap)

containsVar :: Variable -> Assumption -> Bool
containsVar x (HasType y _) | x == y = True
containsVar x (Safe y _) | x == y    = True
containsVar _ _                      = False

singleton :: Variable -> Type -> Assumptions
singleton x t = Assumptions [x `HasType` t]

extend :: Assumptions -> (Variable, Type) -> Assumptions
extend (Assumptions a) (x, t) = Assumptions (x `HasType` t : a)

extendMany :: Assumptions -> [(Variable, Type)] -> Assumptions
extendMany (Assumptions a) bnds =
    Assumptions $ [ x `HasType` t | (x, t) <- bnds ] <> a

makeSafeWrt :: Assumptions -> Capability -> Assumptions
makeSafeWrt (Assumptions a) cap =
    Assumptions $ [ Safe x c | c <- capList cap, HasType x _ <- a ] <> a

remove :: Assumptions -> Variable -> Assumptions
remove (Assumptions a) x = Assumptions irrelevant
    where (_relevant, irrelevant) = partition (containsVar x) a

removeMany :: Assumptions -> [Variable] -> Assumptions
removeMany = foldr (flip remove)

removeWrt :: Assumptions -> (Variable, Capability) -> Assumptions
removeWrt (Assumptions as) (x, cap) = Assumptions irrelevant
  where
    (_relevant, irrelevant) =
        partition (\a -> containsVar x a && not (isSafeWrt cap a)) as

removeManyWrt :: Assumptions -> [(Variable, Capability)] -> Assumptions
removeManyWrt = foldr (flip removeWrt)

removeType :: Assumptions -> Variable -> Assumptions
removeType (Assumptions as) x = Assumptions relevant
  where
    (relevant, _irrelevant) =
        partition (\a -> not (containsVar x a) || isSafeA a) as

removeTypeMany :: Assumptions -> [Variable] -> Assumptions
removeTypeMany = foldr (flip removeType)

-- | Finds all 'Type' in 'Assumption' paired with a given @v@
lookupType :: Variable -> Assumptions -> [Type]
lookupType x as = snd <$> filter ((== x) . fst) (typeAssumptions as)

keys :: Assumptions -> [Variable]
keys as = nub $ fst <$> typeAssumptions as

keysSet :: Assumptions -> S.Set Variable
keysSet as = S.fromList $ fst <$> typeAssumptions as

safeSet :: Assumptions -> S.Set (Variable, BaseCapability)
safeSet as = S.fromList $ safe as
