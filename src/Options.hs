{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

-- | This module takes care of parsing and storing command-line arguments.
module Options
    ( Command(..)
    , Options
    , optSource
    , optVerbose
    , optCommand
    , parseOptions
    ) where

import           Control.Lens
import           Options.Applicative

-- | All possible commands supported by Cocobolo
data Command = Parse
             | Lower
             | Infer
  deriving stock (Show, Eq, Ord)

-- | All possible options supported by Cocobolo
data Options = Options
    { _optCommand :: !Command
    , -- ^ the chosen command
      _optSource  :: !FilePath
    , -- ^ chosen input file
      _optVerbose :: !Bool
      -- ^ verbosity switch
    }
    deriving stock (Show, Eq, Ord)
makeLenses ''Options

-- | Parses 'Options' from command-line inputs
parseOptions :: IO Options
parseOptions = execParser optionsParser

-- | An applicative parser for 'Options' made using the 'Options.Applicative' library
optionsParser :: ParserInfo Options
optionsParser = info
    ((Options <$> commandOption <*> sourceOption <*> verboseOption) <**> helper)
    (  fullDesc
    <> progDesc "cocobolo: programming language with granular safety"
    <> header
           "cocobolo - a proof-of-concept implementation of a programming language with granular safety"
    )
  where
    commandOption = hsubparser (commandParse <> commandLower <> commandInfer)
    commandParse  = command
        "parse"
        (info (pure Parse) (progDesc "Just parse the Cocobolo program"))
    commandLower = command
        "lower"
        (info
            (pure Lower)
            (progDesc
                "Parse the Cocobolo program and lower it into the core CapC system"
            )
        )
    commandInfer = command
        "infer"
        (info
            (pure Infer)
            (progDesc "Parse, lower a Cocobolo program and infer the types")
        )

    sourceOption :: Parser FilePath
    sourceOption =
        strArgument (metavar "SOURCE_FILE" <> help "Path to the source file")

    verboseOption :: Parser Bool
    verboseOption =
        switch
            (short 'v' <> long "verbose" <> help
                "Verbose (debug) output to stdin"
            )

