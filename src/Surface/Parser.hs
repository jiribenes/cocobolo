{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.Text.IO                  as TIO

import           Capability
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Set                      as S
import           Surface.Lexer
import           Surface.Loc
import           Surface.Surface
import           Syntax                         ( Case(..)
                                                , Literal(..)
                                                , Pattern(..)
                                                )

loc :: Parser a -> Parser (Loc a)
loc p = do
    start  <- getSourcePos
    result <- p
    end    <- getSourcePos
    pure $ Loc result (newRangeUnchecked start end)

literal :: Parser Literal
literal = choice
    [ IntLiteral <$> integer
    , BoolLiteral <$> bool
    , TextLiteral <$> text
    , NilLiteral <$ brackets spaceConsumer
    , UnitLiteral <$ try emptyParens
    ]

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

prefixSafe :: (Loc Capability -> a -> a) -> C.Operator Parser a
prefixSafe ctor = C.Prefix $ do
    cap <- loc $ do
        _ <- keyword "safe"
        optional $ chevrons $ Capability . S.fromList <$> sepBy
            (BaseCapability <$> capIdentifier)
            (symbol ",")
    let cap' = fromMaybe noCap <$> cap
    pure $ \rest -> ctor cap' rest

data Assoc = LAssoc | RAssoc | NAssoc

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

fnCall :: Parser a -> Parser [a]
fnCall p = parens (sepBy p (symbol ","))

expr :: Parser Expr
expr = C.makeExprParser term operatorTable

exit :: Parser Expr
exit = do
    Loc _ range <- loc $ keyword "exit"
    _           <- symbol "("
    body        <- expr
    _           <- symbol ")"
    pure $ Exit body range

annot :: Parser (Maybe Type)
annot = optional (symbol ":" *> parseType)

param :: Parser Param
param = Param <$> safety <*> loc identifier <*> annot

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

vardecl :: Parser Expr
vardecl = do
    _           <- keyword "var"
    name        <- loc identifier
    Loc _ range <- loc $ symbol ":="
    body        <- expr
    _           <- keyword "in"
    rest        <- expr
    pure $ VarDecl name body rest range

lam :: Parser Expr
lam = do
    _      <- keyword "fun" <|> lambda
    params <- fnCall param
    _      <- doubleArrow
    body   <- expr
    pure $ Lam params body

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

parseSimplePattern :: Parser (Pattern Id)
parseSimplePattern = choice
    [ parens parsePattern
    , PatternBlank <$ symbol "_"
    , PatternLiteral <$> literal
    , PatternVariable <$> loc identifier
    ]

patternOperatorTable :: [[C.Operator Parser (Pattern Id)]]
patternOperatorTable =
    [ [prefixSafe (PatternSafe . locThing)]
    , [binaryRAssoc doubleColon PatternCons]
    ]
    where binaryRAssoc p make = C.InfixR (make <$ p)

parsePattern :: Parser (Pattern Id)
parsePattern = C.makeExprParser parseSimplePattern patternOperatorTable

parseMatch :: Parser Expr
parseMatch = do
    _      <- keyword "match"
    target <- expr
    _      <- keyword "with"
    arms   <- many (symbol "|" *> parseMatchArm)
    pure $ Match target arms
    where parseMatchArm = Case <$> parsePattern <*> (doubleArrow *> expr)

parseSimpleType :: Parser Type
parseSimpleType = choice
    [ parens parseType
    , TypeConstructor <$> loc typeIdentifier
    , TypeVariable <$> loc identifier
    ]

parseType :: Parser Type
parseType = C.makeExprParser parseSimpleType typeOperatorTable

typeOperatorTable :: [[C.Operator Parser Type]]
typeOperatorTable = [[binaryRAssoc arrow TypeArrow]]
    where binaryRAssoc p make = C.InfixR (make <$ p)

prog :: Parser [Decl]
prog = many (spaceConsumer *> decl <* spaceConsumer <* many newline)

newtype ParseException = ParseException (ParseErrorBundle Text Void)

instance Show ParseException where
    show = coerce errorBundlePretty

instance Exception ParseException where
    displayException = coerce errorBundlePretty

parse :: String -> Text -> IO [Decl]
parse filename contents =
    case Text.Megaparsec.parse (prog <* eof) filename contents of
        Left  err -> throwIO (ParseException err)
        Right x   -> pure x

parseFile :: String -> IO [Decl]
parseFile filename = TIO.readFile filename >>= Surface.Parser.parse filename
