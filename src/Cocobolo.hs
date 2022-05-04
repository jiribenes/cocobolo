module Cocobolo
    ( runCocobolo
    ) where

import           Prettyprinter                  ( pretty )
import qualified Prettyprinter                 as PP
import           Prettyprinter.Render.Text      ( putDoc )

import           Control.Lens                   ( (^.) )
import qualified Data.Text.IO                  as TIO
import           System.Directory               ( doesFileExist )
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )

import           Control.Monad                  ( unless
                                                , when
                                                )
import qualified Infer
import qualified Lower
import           Options
import qualified Surface.Parser                as Parser
import           Surface.Surface                ( Decl )
import           Syntax                         ( Definition
                                                , EffectDefinition
                                                )
import           Type                           ( Scheme )

runCocobolo :: Options -> IO ()
runCocobolo opts = do
    decls <- parse opts
    when (opts ^. optCommand == Parse) exitSuccess

    (defs, effs) <- lower opts decls
    when (opts ^. optCommand == Lower) exitSuccess

    _typedDefs <- infer opts defs effs
    when (opts ^. optCommand == Infer) exitSuccess

    putStrLn "Internal error: invalid command"
    putStrLn
        "You probably added a new command in src/Options and forgot to use it in src/Cocobolo!"
    exitFailure

parse :: Options -> IO [Decl]
parse opts = do
    let sourceFile = opts ^. optSource
    sourceFileExists <- doesFileExist sourceFile

    unless sourceFileExists $ putStrLn "Error: invalid source file!"

    fileContents <- TIO.readFile sourceFile

    decls        <- Parser.parse sourceFile fileContents
    when (opts ^. optVerbose || (opts ^. optCommand) == Parse) $ do
        prettyPrint decls
    pure decls

lower :: Options -> [Decl] -> IO ([Definition ()], [EffectDefinition])
lower opts decls = do
    (defs, effs) <- case Lower.lower decls of
        Right loweredDefs -> pure loweredDefs
        Left  err         -> do
            putStrLn "[ERROR] Encountered an error during lowering:"
            putDoc $ PP.align $ pretty err
            exitFailure

    when (opts ^. optVerbose || opts ^. optCommand == Lower) $ do
        prettyPrint defs
        unless (null effs) $ do
            prettyPrint effs

    pure (defs, effs)

infer
    :: Options
    -> [Definition ()]
    -> [EffectDefinition]
    -> IO [Definition Scheme]
infer opts defs effs = do
    typedDefs <- case Infer.infer effs defs of
        Right typedDefs -> pure typedDefs
        Left  err       -> do
            putStrLn "[ERROR] Encountered an error during type inference:"
            putDoc $ PP.align $ pretty err
            exitFailure

    when (opts ^. optVerbose || opts ^. optCommand == Infer) $ do
        prettyPrint typedDefs

    pure typedDefs

prettyPrint :: PP.Pretty a => [a] -> IO ()
prettyPrint xs = do
    putDoc $ PP.align $ PP.vcat $ pretty <$> xs
    putStrLn ""
    putStrLn ""
