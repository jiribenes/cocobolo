{-# LANGUAGE OverloadedStrings #-}

module Surface.Lexer where

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

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) lineComment blockComment
  where
    lineComment  = L.skipLineComment "//"
    blockComment = L.skipBlockCommentNested "/*" "*/" -- let's not repeat mistakes of the past :)

-- equivalent to @(<* spaceConsumer)@
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- equivalent to @(lexeme spaceConsumer . string)@
symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

emptyParens :: Parser ()
emptyParens = void (symbol "(" *> symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

chevrons :: Parser a -> Parser a
chevrons = between (symbol "<") (symbol ">")

arrow :: Parser Text
arrow = symbol "->" <|> lexeme (T.singleton <$> char '→')

lambda :: Parser Text
lambda = symbol "\\" <|> lexeme (T.singleton <$> char 'λ')

doubleArrow :: Parser Text
doubleArrow = symbol "=>" <|> lexeme (T.singleton <$> char '⇒')

doubleColon :: Parser Text
doubleColon = symbol "::" <|> lexeme (T.singleton <$> char '⸬')

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

typeIdentifier :: Parser Text
typeIdentifier =
    lexeme
        $   T.cons
        <$> satisfy Char.isUpper
        <*> takeWhileP Nothing Char.isAlphaNum

capIdentifier :: Parser Text
capIdentifier =
    lexeme
        $   T.cons
        <$> satisfy Char.isLower
        <*> takeWhileP Nothing Char.isAlphaNum


-- There is no check that the parsed "keyword" is _really_ a keyword.
-- But it's nice :)
keyword :: Text -> Parser Text
keyword = symbol

integer :: Parser Int
integer = lexeme L.decimal

bool :: Parser Bool
bool = choice [True <$ keyword "true", False <$ keyword "false"]

text :: Parser Text
text = T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))
