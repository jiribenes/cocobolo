{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Surface.Loc where

import           Data.Data                      ( Data )
import           GHC.Generics                   ( Generic )
import           Prettyprinter                  ( Pretty(pretty) )
import           Text.Megaparsec.Pos            ( SourcePos(..)
                                                , unPos
                                                )

data Range = Range
    { rangeStart :: SourcePos
    , rangeEnd   :: SourcePos
    , rangeFile  :: String
    }
    deriving stock (Eq, Show, Ord, Data, Generic)

instance Pretty Range where
    pretty (Range (SourcePos _ startLine startCol) (SourcePos _ endLine endCol) file)
        | startLine == endLine
        = pretty file
            <> ":"
            <> pretty (unPos startLine)
            <> ":"
            <> pretty (unPos startCol)
            <> "-"
            <> pretty (unPos endCol)
        | otherwise
        = pretty file
            <> ":"
            <> pretty (unPos startLine)
            <> ":"
            <> pretty (unPos startCol)
            <> "-"
            <> pretty (unPos endLine)
            <> ":"
            <> pretty (unPos endCol)

newRange :: SourcePos -> SourcePos -> Maybe Range
newRange a b | sourceName a == sourceName b = Just (newRangeUnchecked a b)
             | otherwise                    = Nothing

newRangeUnchecked :: SourcePos -> SourcePos -> Range
newRangeUnchecked a b = Range start end (sourceName a)
  where
    start = min a b
    end   = max a b

instance Semigroup Range where
    Range a _ l <> Range _ b _ = Range a b l

includes :: Range -> Range -> Bool
includes (Range bigStart bigEnd l) (Range smallStart smallEnd l') =
    l == l' && smallStart >= bigStart && smallEnd <= bigEnd

data Loc a = Loc
    { locThing :: a
    , locRange :: Range
    }
    deriving stock (Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

instance Show a => Show (Loc a) where
    show = show . locThing

instance Pretty a => Pretty (Loc a) where
    pretty = pretty . locThing
