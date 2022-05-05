{-# LANGUAGE OverloadedStrings #-}

-- | The parser module for Cocobolo (surface) syntax.
-- Uses the @megaparsec@ parser combinator library.
module Surface.Parser
    ( Surface.Parser.parse
    , Surface.Parser.parseFile
    ) where

import qualified Control.Monad.Combinators.Expr
                                               as C
import           Data.Text                      ( Text )
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Control.Exception              ( Exception
                                                , throwIO
                                                )
import           Control.Exception.Base         ( Exception(displayException) )
import           Data.Coerce                    ( coerce )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Set                      as S
import qualified Data.Text.IO                  as TIO

import           Capability
import           Surface.Lexer
import           Surface.Loc
import           Surface.Surface
import           Syntax                         ( Case(..)
                                                , Literal(..)
                                                , Pattern(..)
                                                )

-- | Helpful combinator for parsing a located term.
loc :: Parser a -> Parser (Loc a)
loc p = do
    start  <- getSourcePos
    result <- p
    end    <- getSourcePos
    pure $ Loc result (newRangeUnchecked start end)

-- | Parser for 'Literal's.
literal :: Parser Literal
literal = choice
    [ IntLiteral <$> integer
    , BoolLiteral <$> bool
    , TextLiteral <$> text
    , NilLiteral <$ brackets spaceConsumer
    , UnitLiteral <$ try emptyParens
    ]

-- | Parser for basic expressions (terms).
term :: Parser Expr
term = choice
    [ Literal <$> literal
    , parens expr
    , parseMatch
    , letin
    , vardecl
    , lam
    , Identifier <$> loc identifier
    , exit
    ]

-- | Helpful function for parsing a `safe<...>` prefix
-- returning a 'C.Operator'.
prefixSafe :: (Loc Capability -> a -> a) -> C.Operator Parser a
prefixSafe ctor = C.Prefix $ do
    cap <- loc $ do
        _ <- keyword "safe"
        optional $ chevrons $ Capability . S.fromList <$> sepBy
            (BaseCapability <$> capIdentifier)
            (symbol ",")
    let cap' = fromMaybe noCap <$> cap
    pure $ \rest -> ctor cap' rest

-- | Handy data type for denoting associativity of an operation.
data Assoc = LAssoc | RAssoc | NAssoc

-- | Table of operators which is later used for parsing expressions.
-- Denotes associativity, arity and precedence.
operatorTable :: [[C.Operator Parser Expr]]
operatorTable =
    [ [postfixFnCall]
    , [prefixSafe (Unary . fmap Safe)]
    , [binaryOp Assign NAssoc (symbol ":=")]
    , [infixSeq]
    , [prefixOp Deref (symbol "!")]
    , [binaryOp Cons RAssoc doubleColon]
    , [binaryOp Mul LAssoc (symbol "*"), binaryOp Div LAssoc (symbol "/")]
    , [binaryOp Add LAssoc (symbol "+"), binaryOp Sub LAssoc (symbol "-")]
    ]
  where
    binaryOp :: BinOp -> Assoc -> Parser a -> C.Operator Parser Expr
    binaryOp op LAssoc p = C.InfixL $ do
        result <- loc p
        pure $ Binary (op <$ result)
    binaryOp op RAssoc p = C.InfixR $ do
        result <- loc p
        pure $ Binary (op <$ result)
    binaryOp op NAssoc p = C.InfixN $ do
        result <- loc p
        pure $ Binary (op <$ result)

    prefixOp :: UnOp -> Parser a -> C.Operator Parser Expr
    prefixOp op p = C.Prefix $ do
        result <- loc p
        pure $ Unary $ op <$ result

    infixSeq :: C.Operator Parser Expr
    infixSeq = C.InfixL $ do
        Loc _ range <- loc $ symbol ";"
        pure (\e1 e2 -> Seq e1 e2 range)

    postfixFnCall :: C.Operator Parser Expr
    postfixFnCall = C.Postfix $ do
        args <- fnCall expr
        pure $ \fn -> Call fn args

-- | Parser for a function call.
fnCall :: Parser a -> Parser [a]
fnCall p = parens (sepBy p (symbol ","))

-- | Expression parser using the operators defined in 'operatorTable'
-- with a 'term' as a basic unit.
expr :: Parser Expr
expr = C.makeExprParser term operatorTable

-- | Parses an exit expression.
exit :: Parser Expr
exit = do
    Loc _ range <- loc $ keyword "exit"
    _           <- symbol "("
    body        <- expr
    _           <- symbol ")"
    pure $ Exit body range

-- | Parses an optional type annotation.
annot :: Parser (Maybe Type)
annot = optional (symbol ":" *> parseType)

-- | Parses a parameter ('Param').
param :: Parser Param
param = Param <$> safety <*> loc identifier <*> annot

-- | Parses the general 'Safety' type.
safety :: Parser Safety
safety = do
    result <- optional $ do
        _ <- keyword "safe"
        optional $ chevrons $ Capability . S.fromList <$> sepBy
            (BaseCapability <$> capIdentifier)
            (symbol ",")

    pure $ case result of
        Nothing       -> ImplicitUnsafe
        Just maybeCap -> case maybeCap of
            Nothing  -> ExplicitSafe noCap
            Just cap -> ExplicitSafe cap

-- | Parses a let expression.
letin :: Parser Expr
letin = do
    _      <- keyword "let"
    s      <- safety
    name   <- loc identifier
    params <- optional (fnCall param)
    _      <- symbol "="
    body   <- expr
    _      <- keyword "in"
    rest   <- expr
    pure $ LetIn s name params body rest

-- | Parses a variable declaration expression.
vardecl :: Parser Expr
vardecl = do
    _           <- keyword "var"
    name        <- loc identifier
    Loc _ range <- loc $ symbol ":="
    body        <- expr
    _           <- keyword "in"
    rest        <- expr
    pure $ VarDecl name body rest range

-- | Parses a lambda abstraction.
lam :: Parser Expr
lam = do
    _      <- keyword "fun" <|> lambda
    params <- fnCall param
    _      <- doubleArrow
    body   <- expr
    pure $ Lam params body

-- | Parses a declaration ('Decl').
decl :: Parser Decl
decl = choice [parseLet, parseEffect]
  where
    parseLet = do
        _          <- keyword "let"
        s          <- safety
        name       <- loc identifier
        params     <- optional (fnCall param)
        maybeAnnot <- annot
        _          <- symbol "="
        body       <- expr
        pure $ Let s name params maybeAnnot body

    parseEffect = do
        _    <- keyword "effect"
        name <- loc capIdentifier
        _    <- keyword "with"
        arms <- many effectArm
        pure $ Effect name arms

    effectArm = do
        _          <- symbol "|"
        name       <- loc identifier
        params     <- optional (fnCall param)
        maybeAnnot <- annot
        pure (name, params, maybeAnnot)

-- | Parser for basic patterns ('Pattern').
parseSimplePattern :: Parser (Pattern Id)
parseSimplePattern = choice
    [ parens parsePattern
    , PatternBlank <$ symbol "_"
    , PatternLiteral <$> literal
    , PatternVariable <$> loc identifier
    ]

-- | Operator table for patterns.
--
-- See more details about operator tables in the documentation for 'operatorTable'.
patternOperatorTable :: [[C.Operator Parser (Pattern Id)]]
patternOperatorTable =
    [ [prefixSafe (PatternSafe . locThing)]
    , [binaryRAssoc doubleColon PatternCons]
    ]
    where binaryRAssoc p make = C.InfixR (make <$ p)

-- | General pattern parser, uses the operator table 'patternOperatorTable'
-- and 'parseSimplePattern' as a basic expression.
parsePattern :: Parser (Pattern Id)
parsePattern = C.makeExprParser parseSimplePattern patternOperatorTable

-- | Parses a pattern matching expression.
parseMatch :: Parser Expr
parseMatch = do
    _      <- keyword "match"
    target <- expr
    _      <- keyword "with"
    arms   <- many (symbol "|" *> parseMatchArm)
    pure $ Match target arms
    where parseMatchArm = Case <$> parsePattern <*> (doubleArrow *> expr)

-- | Parses a basic type ('Type').
parseSimpleType :: Parser Type
parseSimpleType = choice
    [ parens parseType
    , TypeList <$> brackets parseType
    , TypeConstructor <$> loc typeIdentifier
    , TypeVariable <$> loc identifier
    ]

-- | Parse a general type using 'typeOperatorTable' operator table
-- and 'parseSimpleType' as the basic building block.
parseType :: Parser Type
parseType = C.makeExprParser parseSimpleType typeOperatorTable

-- | Operator table for types.
--
-- See more details about operator tables in the documentation for 'operatorTable'.
typeOperatorTable :: [[C.Operator Parser Type]]
typeOperatorTable =
    [[prefixOp (symbol "^") TypeRef], [binaryRAssoc arrow TypeArrow]]
  where
    binaryRAssoc p make = C.InfixR (make <$ p)
    prefixOp p make = C.Prefix (make <$ p)

-- | Top-level parser of declarations.
prog :: Parser [Decl]
prog = many (spaceConsumer *> decl <* spaceConsumer <* many newline)

-- | Thin wrapper over 'ParseErrorBundle'
-- to signify a lexer/parser error.
newtype ParseException = ParseException (ParseErrorBundle Text Void)

instance Show ParseException where
    show = coerce errorBundlePretty

instance Exception ParseException where
    displayException = coerce errorBundlePretty

-- | Top-level parsing function.
parse :: String -> Text -> IO [Decl]
parse filename contents =
    case Text.Megaparsec.parse (prog <* eof) filename contents of
        Left  err -> throwIO (ParseException err)
        Right x   -> pure x

-- | Top-level parsing function for a given filename.
-- Reads the file and then calls 'parse'.
parseFile :: String -> IO [Decl]
parseFile filename = TIO.readFile filename >>= Surface.Parser.parse filename
