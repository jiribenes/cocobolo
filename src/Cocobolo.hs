module Cocobolo
    ( runCocobolo
    ) where

import           Prettyprinter                  ( pretty )
import qualified Prettyprinter                 as PP
import           Prettyprinter.Render.Text      ( putDoc )

import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as TIO

import           Control.Monad                  ( unless )
import qualified Infer
import qualified Lower
import qualified Surface.Parser                as Parser

runCocobolo :: IO ()
runCocobolo = putStrLn "TODO TODO TODO (use runFile or runTopLevel)"

runTopLevel :: FilePath -> Text -> IO ()
runTopLevel filename contents = do
    decls <- Parser.parse filename contents
    putDoc $ PP.align $ PP.vcat $ pretty <$> decls
    putStrLn ""
    putStrLn ""

    (defs, effs) <- case Lower.lower decls of
        Right loweredDefs -> pure loweredDefs
        Left  err         -> do
            putStrLn "[ERROR] Encountered an error during lowering:"
            putDoc $ PP.align $ pretty err
            pure ([], [])
    putDoc $ PP.align $ PP.vcat $ pretty <$> defs

    unless (null effs) $ do
        putStrLn ""
        putDoc $ PP.align $ PP.vcat $ pretty <$> effs
    putStrLn ""
    putStrLn ""

    typedDefs <- case Infer.infer effs defs of
        Right typedDefs -> pure typedDefs
        Left  err       -> do
            putStrLn "[ERROR] Encountered an error during type inference:"
            putDoc $ PP.align $ pretty err
            pure []
    putDoc $ PP.align $ PP.vcat $ pretty <$> typedDefs
    putStrLn ""

runFile :: FilePath -> IO ()
runFile filename = TIO.readFile filename >>= runTopLevel filename
