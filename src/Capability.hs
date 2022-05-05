{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Capability
    ( BaseCapability(..)
    , Capability(..)
    , noCap
    , singletonCap
    , capList
    , capMember
    , capDifference
    , reservedCapNames
    ) where

import           Data.Data                      ( Data )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Prettyprinter                  ( Pretty(pretty) )
import qualified Prettyprinter                 as PP

-- | Represents a base set of capabilities.
newtype BaseCapability = BaseCapability { unBaseCapability :: Text }
    deriving stock (Eq, Ord, Show, Data, Generic)

instance Pretty BaseCapability where
    pretty (BaseCapability c) = pretty c

-- | Represents a set of 'BaseCapability' --- a set of various capabilities
newtype Capability = Capability { unCapability :: S.Set BaseCapability }
    deriving stock (Eq, Ord, Show, Data, Generic)

instance Pretty Capability where
    pretty (Capability s) =
        PP.encloseSep mempty mempty PP.comma (map pretty (S.toList s))

-- | Returns the empty set of capabilities
noCap :: Capability
noCap = Capability mempty

-- | Creates a set of capabilities from a single base capability
singletonCap :: Text -> Capability
singletonCap = Capability . S.singleton . BaseCapability

-- | Unwraps a set of capabilities into a list of 'BaseCapability'
capList :: Capability -> [BaseCapability]
capList = S.toList . unCapability

-- | Checks if the given 'BaseCapability' is a member of the given 'Capability'
capMember :: BaseCapability -> Capability -> Bool
capMember b (Capability s) = S.member b s

-- | Computes the set difference of two capability sets
capDifference :: Capability -> Capability -> Capability
capDifference (Capability allCaps) (Capability someCaps) =
    Capability $ S.difference allCaps someCaps

-- | Reserved names of the base capabilities of the standard library.
--
-- Used during lowering to check that an effect is not overwritten.
reservedCapNames :: S.Set Text
reservedCapNames = S.fromList ["ref", "exit"]
