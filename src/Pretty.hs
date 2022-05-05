-- | A module for pretty-printing utilities
module Pretty
    ( withIndentPerhaps
    , prettyPrint
    ) where

import qualified Prettyprinter                 as P
import qualified Prettyprinter.Render.Text     as P

-- | Inserts either a newline and an indent
-- or a plain space.
--
-- Useful for formatting large blocks!
withIndentPerhaps :: P.Doc ann -> P.Doc ann
withIndentPerhaps doc = P.group (P.flatAlt (niceIndent doc) (P.space <> doc))
  where
    niceIndent :: P.Doc ann -> P.Doc ann
    niceIndent doc' = P.hardline P.<+> P.indent 3 doc'

-- | A helpful function that pretty-prints a list with vertical spaces into the standard output
prettyPrint :: P.Pretty a => [a] -> IO ()
prettyPrint xs = do
    P.putDoc $ P.align $ P.vcat $ P.pretty <$> xs
    putStrLn ""
    putStrLn ""
