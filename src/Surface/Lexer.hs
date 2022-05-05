{-# LANGUAGE OverloadedStrings #-}

-- | This module contains a sort-of lexer 
-- written with the @megaparsec@ parser combinator library.
module Surface.Lexer
    ( Parser
    , spaceConsumer
    , lexeme
    , symbol
    , parens
    , emptyParens
    , brackets
    , chevrons
    , arrow
    , lambda
    , doubleArrow
    , doubleColon
    , identifier
    , typeIdentifier
    , capIdentifier
    , keyword
    , integer
    , bool
    , text
    ) where

import           Control.Monad                  ( void
                                                , when
                                                )
import qualified Data.Char                     as Char
import           Data.HashSet                   ( HashSet )
import qualified Data.HashSet                  as HashSet
import           Data.List.NonEmpty             ( NonEmpty((:|)) )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void                      ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char           ( char
                                                , spaceChar
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void Text

-- | A basic parser which consumes whitespace
spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) lineComment blockComment
  where
    lineComment  = L.skipLineComment "//"
    blockComment = L.skipBlockCommentNested "/*" "*/" -- let's not repeat mistakes of the past :)

-- | Used to define a lexical token
-- while respecting whitespace.
--
-- equivalent to @(<* spaceConsumer)@
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | Used to define a symbolic lexical token
-- while respecting whitespace.
-- 
-- equivalent to @(lexeme spaceConsumer . string)@
symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

-- | Combinator for parsing parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parser for @()@
emptyParens :: Parser ()
emptyParens = void (symbol "(" *> symbol ")")

-- | Combinator for parsing brackets
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Combinator for parsing chevrons
chevrons :: Parser a -> Parser a
chevrons = between (symbol "<") (symbol ">")

-- | Parser for a (potentially unicode) arrow
arrow :: Parser Text
arrow = symbol "->" <|> lexeme (T.singleton <$> char '→')

-- | Parser for a (potentially unicode) lambda
lambda :: Parser Text
lambda = symbol "\\" <|> lexeme (T.singleton <$> char 'λ')

-- | Parser for a (potentially unicode) double arrow 
doubleArrow :: Parser Text
doubleArrow = symbol "=>" <|> lexeme (T.singleton <$> char '⇒')

-- | Parser for a (potentially unicode) double colon 
doubleColon :: Parser Text
doubleColon = symbol "::" <|> lexeme (T.singleton <$> char '⸬')

-- | Creates a parser for identifiers that ignores given keywords
makeIdentifierParser
    :: NonEmpty Char -> Parser Text -> HashSet Text -> Parser Text
makeIdentifierParser lbl p keywords = do
    x <- lookAhead p
    guardNotKeyword x
    _ <- p
    pure x
  where
    guardNotKeyword :: Text -> Parser ()
    guardNotKeyword x = when (x `HashSet.member` keywords) $ do
        failure (Just (Tokens (T.head x :| T.unpack (T.tail x))))
                (Set.singleton (Label lbl))

-- | Parser for identifiers
identifier :: Parser Text
identifier = makeIdentifierParser ('a' :| "n identifier") p keywords
  where
    keywords :: HashSet Text
    keywords = HashSet.fromList
        [ "data"
        , "safe"
        , "def"
        , "let"
        , "var"
        , "true"
        , "false"
        , "in"
        , "if"
        , "else"
        , "match"
        , "with"
        , "exit"
        , "unbox"
        , "effect"
        ]

    p :: Parser Text
    p =
        lexeme
            $   T.cons
            <$> satisfy Char.isLower
            <*> takeWhileP Nothing Char.isAlphaNum

-- | Parser for type identifiers
typeIdentifier :: Parser Text
typeIdentifier =
    lexeme
        $   T.cons
        <$> satisfy Char.isUpper
        <*> takeWhileP Nothing Char.isAlphaNum

-- | Parser for capability identifiers
capIdentifier :: Parser Text
capIdentifier =
    lexeme
        $   T.cons
        <$> satisfy Char.isLower
        <*> takeWhileP Nothing Char.isAlphaNum


-- | Parses a keyword.
--
-- There is no check that the parsed "keyword" is _really_ a keyword.
-- But it's still useful for reading the code @:)@
keyword :: Text -> Parser Text
keyword = symbol

-- | Parses an integral constant.
integer :: Parser Int
integer = lexeme L.decimal

-- | Parses a boolean expression.
bool :: Parser Bool
bool = choice [True <$ keyword "true", False <$ keyword "false"]

-- | Parses a string denoted by double quotes.
text :: Parser Text
text = T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))
