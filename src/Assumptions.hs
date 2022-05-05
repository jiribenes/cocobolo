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

import           Data.List                      ( nub )
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

-- | Constraint-based type system assumption.
-- Describes a single piece of information assumed by the type system.
-- 
-- Unlike in the thesis, we split the safety information and the type information
-- as it streamlines the implementation.
data Assumption
    = HasType Variable Type
      -- ^ A variable 'Variable' has the type 'Type'
    | Safe Variable BaseCapability
      -- ^ A variable 'Variable' is safe with respect to the base capability 'BaseCapability'
  deriving stock (Show, Eq, Ord, Generic)

-- | Checks if the 'Assumption' is an assumption about safety
isSafe :: Assumption -> Bool
isSafe HasType{} = False
isSafe Safe{}    = True

-- | Checks if the 'Assumption' is safe with respect to the given set of capabilities 'Capability'
isSafeWrt :: Capability -> Assumption -> Bool
isSafeWrt cap (Safe _ baseCap) | not (baseCap `capMember` cap) = True
isSafeWrt _ _ = False

-- | A multimap of 'Assumption's
newtype Assumptions = Assumptions { unAssumptions :: [Assumption] }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Semigroup, Monoid)

-- | Returns all of the type assumptions (all @HasType x t@)
typeAssumptions :: Assumptions -> [(Variable, Type)]
typeAssumptions (Assumptions a) = mapMaybe go a
  where
    go (HasType x t) = Just (x, t)
    go Safe{}        = Nothing

-- | Returns all safety assumptions (all @Safe x c@)
safe :: Assumptions -> [(Variable, BaseCapability)]
safe (Assumptions a) = mapMaybe go a
  where
    go (HasType _ _      ) = Nothing
    go (Safe    x baseCap) = Just (x, baseCap)

-- | Checks if the assumption contains a variable
containsVar :: Variable -> Assumption -> Bool
containsVar x (HasType y _) | x == y = True
containsVar x (Safe y _) | x == y    = True
containsVar _ _                      = False

-- | Creates a new assumption multimap with the sole
-- information that 'Variable' has type 'Type'
singleton :: Variable -> Type -> Assumptions
singleton x t = Assumptions [x `HasType` t]

-- | Extends an assumption set with a type assumption
extend :: Assumptions -> (Variable, Type) -> Assumptions
extend (Assumptions a) (x, t) = Assumptions (x `HasType` t : a)

-- | Extends an assumption set with multiple type assumptions
extendMany :: Assumptions -> [(Variable, Type)] -> Assumptions
extendMany (Assumptions a) bnds =
    Assumptions $ [ x `HasType` t | (x, t) <- bnds ] <> a

-- | For every type assumption in the multiset,
-- this function creates a new assumption that the variable is safe.
--
-- Corresponds to purification in the thesis.
makeSafeWrt :: Assumptions -> Capability -> Assumptions
makeSafeWrt (Assumptions a) cap =
    Assumptions $ [ Safe x c | c <- capList cap, HasType x _ <- a ] <> a

-- | Removes a variable from the assumption multiset
remove :: Assumptions -> Variable -> Assumptions
remove (Assumptions a) x = Assumptions $ filter (not . containsVar x) a

-- | Removes multiple variables from the assumption multiset
removeMany :: Assumptions -> [Variable] -> Assumptions
removeMany = foldr (flip remove)

-- | Removes capability assumptions for a given variable
removeWrt :: Assumptions -> (Variable, Capability) -> Assumptions
removeWrt (Assumptions as) (x, cap) =
    Assumptions $ filter (\a -> not (containsVar x a) || isSafeWrt cap a) as

-- | Removes capability assumptions for many given variables
removeManyWrt :: Assumptions -> [(Variable, Capability)] -> Assumptions
removeManyWrt = foldr (flip removeWrt)

-- | Removes a type assumption for a given variable
removeType :: Assumptions -> Variable -> Assumptions
removeType (Assumptions as) x =
    Assumptions $ filter (\a -> not (containsVar x a) || isSafe a) as

-- | Removes a type assumption for many given variables
removeTypeMany :: Assumptions -> [Variable] -> Assumptions
removeTypeMany = foldr (flip removeType)

-- | Finds all 'Type' in 'Assumption' paired with a given @v@
lookupType :: Variable -> Assumptions -> [Type]
lookupType x as = snd <$> filter ((== x) . fst) (typeAssumptions as)

-- | Returns the variables mentioned in type assumptions without any duplicates
keys :: Assumptions -> [Variable]
keys as = nub $ fst <$> typeAssumptions as

-- | Returns the variables mentioned in type assumptions as a 'S.Set'
keysSet :: Assumptions -> S.Set Variable
keysSet as = S.fromList $ fst <$> typeAssumptions as

-- | Returns the safety assumptions as a 'S.Set'
safeSet :: Assumptions -> S.Set (Variable, BaseCapability)
safeSet as = S.fromList $ safe as
