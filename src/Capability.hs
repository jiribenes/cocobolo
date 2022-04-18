{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Capability where

import           Data.Data                      ( Data )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Prettyprinter                  ( Pretty(pretty) )
import qualified Prettyprinter                 as PP

reservedCapNames :: S.Set Text
reservedCapNames = S.fromList ["ref", "exit"]

newtype BaseCapability = BaseCapability { unBaseCapability :: Text }
    deriving stock (Eq, Ord, Show, Data, Generic)

instance Pretty BaseCapability where
    pretty (BaseCapability c) = pretty c

newtype Capability = Capability { unCapability :: S.Set BaseCapability }
    deriving stock (Eq, Ord, Show, Data, Generic)

instance Pretty Capability where
    pretty (Capability s) =
        PP.encloseSep mempty mempty PP.comma (map pretty (S.toList s))

noCap :: Capability
noCap = Capability mempty

singletonCap :: Text -> Capability
singletonCap = Capability . S.singleton . BaseCapability

capList :: Capability -> [BaseCapability]
capList = S.toList . unCapability

capMember :: BaseCapability -> Capability -> Bool
capMember b (Capability s) = S.member b s

capDifference :: Capability -> Capability -> Capability
capDifference (Capability allCaps) (Capability someCaps) =
    Capability $ S.difference allCaps someCaps
