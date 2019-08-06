{-# language
        LambdaCase
      , OverloadedStrings
      , TemplateHaskell
  #-}

module Roundtrip where

import Control.Monad (void, guard)
import Control.Applicative (Alternative(..))

import Data.Text (Text)
import qualified Data.Text as Text

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Text.Parsec (ParseError)
import qualified Text.Parsec as Parsec
import Text.Parsec.Text (Parser)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- | Tokens in our made-up language.
data Token
  = TInt Int
  | TFloat Double
  | TAssign
  | TPlus
  | TPlusEq
  | TVar Text
  | TLParen
  | TRParen
  | TIncrement
  | TSeparator
  deriving (Eq, Ord, Show, Read)

-- | Render a single token.
renderToken :: Token -> Text
renderToken = \case
  TInt x -> Text.pack (show x)
  TFloat d -> Text.pack (printf "%6f" d)
  TAssign -> ":="
  TPlus -> "+"
  TPlusEq -> "+="
  TVar x -> x
  TLParen -> "("
  TRParen -> ")"
  TIncrement -> "++"
  TSeparator -> ";"

-- | Pretty-print a stream of tokens.
pretty :: [Token] -> Text
pretty = Text.intercalate " " . fmap renderToken

-- | Consume whitespace.
whitespace :: Parser ()
whitespace = void (many Parsec.space)

parseInt :: Parser Int
parseInt = do
  int <- Parsec.many1 Parsec.digit
  case readMaybe int of
    Nothing -> fail "parseInt: Did not parse an integer!"
    Just x -> pure x

parseDouble :: Parser Double
parseDouble = do
  a <- Parsec.many1 Parsec.digit
  _ <- Parsec.char '.'
  b <- Parsec.many1 Parsec.digit
  guard (length b <= 6)
  case readMaybe (a ++ "." ++ b) of
    Nothing -> fail "parseDouble: Did not parse a double!"
    Just x -> pure x

-- | Parse variable names
parseVar :: Parser Text
parseVar = Text.pack <$> Parsec.many1 Parsec.letter

-- | Parse a single token
parseToken :: Parser Token
parseToken = Parsec.choice
  [ TFloat <$> Parsec.try parseDouble
  , TInt <$> parseInt
  , TVar <$> parseVar
  , Parsec.try (Parsec.string "++") *> pure TIncrement
  , Parsec.try (Parsec.string "+=") *> pure TPlusEq
  , Parsec.string ":=" *> pure TAssign
  , Parsec.string "+" *> pure TPlus
  , Parsec.string "(" *> pure TLParen
  , Parsec.string ")" *> pure TRParen
  , Parsec.string ";" *> pure TSeparator
  ]

-- | Parse a stream of tokens.
parseTokens :: Parser [Token]
parseTokens = do
  whitespace
  Parsec.many (parseToken <* whitespace)

-- | Parse a piece of text that represents an expression in our token
--   language.
parse :: Text -> Either ParseError [Token]
parse = Parsec.parse (parseTokens <* Parsec.eof) ""

-----------------------------------------------------------------------

genTokens :: Gen [Token]
genTokens = Gen.list (Range.linear 0 100) genToken

round6 :: Double -> Double
round6 x = fromInteger (round (x * (10 ^ (6 :: Int)))) / 10.0 ^^ (6 :: Int)

genToken :: Gen Token
genToken = Gen.choice
  [ TInt <$> Gen.int (Range.linear 0 maxBound)
  , TFloat . round6 <$> Gen.double (Range.exponentialFloat 0.0 9223372036854775807.9)
  , pure TAssign
  , pure TPlus
  , pure TPlusEq
  , TVar <$> Gen.text (Range.linear 1 20) Gen.alpha
  , pure TLParen
  , pure TRParen
  , pure TIncrement
  , pure TSeparator
  ]

prop_trip :: Property
prop_trip = withTests 1000 . property $ do
  toks <- forAll genTokens
  tripping toks pretty parse

tests :: IO Bool
tests = checkParallel $$(discover)

