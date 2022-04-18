module Pretty where

import qualified Prettyprinter                 as P

niceIndent :: P.Doc ann -> P.Doc ann
niceIndent doc = P.hardline P.<+> P.indent 3 doc

-- | Inserts either a newline and an indent
-- or a plain space.
--
-- Useful for formatting large blocks!
withIndentPerhaps :: P.Doc ann -> P.Doc ann
withIndentPerhaps doc = P.group (P.flatAlt (niceIndent doc) (P.space <> doc))


