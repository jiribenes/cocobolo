{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This tiny module represents a source-code location
module Surface.Loc
    ( Range(..)
    , newRange
    , newRangeUnchecked
    , Loc(..)
    ) where

import           Data.Data                      ( Data )
import           GHC.Generics                   ( Generic )
import           Prettyprinter                  ( Pretty(pretty) )
import           Text.Megaparsec.Pos            ( SourcePos(..)
                                                , unPos
                                                )

-- | Represents a source-code range: two positions in a file and a filename
data Range = Range
    { rangeStart :: SourcePos
    , rangeEnd   :: SourcePos
    , rangeFile  :: String
    }
    deriving stock (Eq, Show, Ord, Data, Generic)

-- | Pretty-printing instance for 'Range',
-- attempts to write a shorthand notation if the two positions are on the same line.
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

-- | Creates a new 'Range', fails if the two 'SourcePos'
-- don't share a filename!
newRange :: SourcePos -> SourcePos -> Maybe Range
newRange a b | sourceName a == sourceName b = Just (newRangeUnchecked a b)
             | otherwise                    = Nothing

-- | Creates a new 'Range' from two 'SourcePos'.
-- Does not check anything, please use 'newRange' instead, if possible.
newRangeUnchecked :: SourcePos -> SourcePos -> Range
newRangeUnchecked a b = Range start end (sourceName a)
  where
    start = min a b
    end   = max a b

instance Semigroup Range where
    Range a _ l <> Range _ b _ = Range a b l

-- | 'Loc' represents a thing together with a 'Range'.
-- Commonly used for AST nodes that hold a location.
data Loc a = Loc
    { locThing :: a
    , locRange :: Range
    }
    deriving stock (Eq, Ord, Data, Generic, Functor, Foldable, Traversable)

instance Show a => Show (Loc a) where
    show = show . locThing

instance Pretty a => Pretty (Loc a) where
    pretty = pretty . locThing
