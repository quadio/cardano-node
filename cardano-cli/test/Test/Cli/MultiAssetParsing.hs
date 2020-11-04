{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cli.MultiAssetParsing where

import           Cardano.Prelude

import           Cardano.CLI.Mary.Parser
import qualified Data.Text as Text
import           Test.Cli.Gen
import           Text.Parsec (ParseError)
import qualified Text.Parsec as Parsec (parse)
import           Text.Parsec.String (Parser)

import           Hedgehog (Property, checkSequential, discover, evalEither, forAll, property, (===))
import           Hedgehog.Internal.Property (failWith)

-- Lexer
lex :: Parser a -> Text -> Either ParseError a
lex p = Parsec.parse p "" . Text.unpack

-- Parser
parse :: TParser a -> Tokens -> Either ParseError a
parse p = Parsec.parse p ""

-- Lexing

prop_lexLovelace :: Property
prop_lexLovelace =
  property $ do
    (input, expectedOutput) <- forAll genLovelaceToken
    case lex lexToken input of
      Left pe -> failWith Nothing $ show pe
      Right token -> token === expectedOutput

prop_lexValue_fullySpecified :: Property
prop_lexValue_fullySpecified =
  property $ do
    (input, expectedOutput) <- forAll genValueTokenFullySpecified
    case lex lexToken input of
      Left pe -> failWith Nothing $ show pe
      Right token -> token === expectedOutput


prop_lexValue_pid_and_asset_id :: Property
prop_lexValue_pid_and_asset_id =
  property $ do
    (input, expectedOutput) <- forAll genValueTokenPidAndAssetId
    case lex lexToken input of
      Left pe -> failWith Nothing $ show pe
      Right token -> token === expectedOutput

prop_lexValue_pid_only :: Property
prop_lexValue_pid_only =
  property $ do
    (input, expectedOutput) <- forAll genValueTokenPidOnly
    case lex lexToken input of
      Left pe -> failWith Nothing $ show pe
      Right token -> token === expectedOutput

prop_lexAddition :: Property
prop_lexAddition =
  property $ do
    (input, expectedOutput) <- forAll genAdditionToken
    case lex lexToken input of
      Left pe -> failWith Nothing $ show pe
      Right token -> token === expectedOutput

prop_lexTokens :: Property
prop_lexTokens =
  property $ do
    (input, expectedOutput) <- forAll genTokens
    case lex lexTokens input of
      Left pe -> failWith Nothing $ show pe
      Right tokens ->
        sort (foldl' (\n (_,tk) ->  tk : n) [] tokens) === sort expectedOutput

-- Parsing

prop_parseLovelace :: Property
prop_parseLovelace =
  property $ do
    (input, expectedOutput) <- forAll genLovelaceValue
    tkn <- evalEither $ lex lexTokens input
    case parse preValueLovelace tkn of
      Left pe -> failWith Nothing $ show pe
      Right preVal -> preValtoValue preVal === expectedOutput

prop_parseMultiAsset :: Property
prop_parseMultiAsset =
  property $ do
      (input, expectedOutput) <- forAll genMultiAssetValue
      tkn <- evalEither $ lex lexTokens input
      case parse preValueMultiAsset tkn of
        Left pe -> failWith Nothing $ show pe
        Right preVal -> preValtoValue preVal === expectedOutput

prop_parseAddition :: Property
prop_parseAddition =
  property $ do
    (input, expectedOutput) <- forAll genAdditionValue
    tkn <- evalEither $ lex lexTokens input
    case parse preValueAddition tkn of
      Left pe -> failWith Nothing $ show pe
      Right preVal -> preValtoValue preVal === expectedOutput

prop_parse :: Property
prop_parse = property $ do
  (input, expectedOutput) <- forAll genValues
  tkns <- evalEither $ lex lexTokens input
  case parse preValueParser tkns of
    Left pe -> failWith Nothing $ show pe
    Right preVals -> calculateValue preVals === expectedOutput

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  checkSequential $$discover

