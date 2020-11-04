{-# LANGUAGE FlexibleContexts #-}

module Cardano.CLI.Mary.Parser
  ( Token (..)
  , Tokens
  , TParser
  , calculateValue
  , lexToken
  , lexTokens
  , preValueAddition
  , preValueLovelace
  , preValueMultiAsset
  , preValueParser
  , preValtoValue
  , stringToValue
  , textToPolicyId
  ) where

import           Cardano.Prelude hiding (try)
import           Prelude (String, read)


import           Control.Monad (fail)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Text.Parsec (ParseError, Parsec, SourcePos, getPosition, parse, token, try, (<?>))
import           Text.Parsec.Char (alphaNum, digit, hexDigit, letter, space, spaces, string)
import           Text.Parsec.String (Parser)
import           Text.ParserCombinators.Parsec.Combinator (eof, many1, manyTill, notFollowedBy,
                     sepBy1)

import           Cardano.Crypto.Hash (hashFromStringAsHex)
import           Cardano.Ledger.Mary (MaryEra)
import           Cardano.Ledger.Mary.Value (AssetID (..), PolicyID (..), Value (..))
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import           Shelley.Spec.Ledger.Scripts (ScriptHash (..))

stringToValue :: String -> Either ParseError (Value (MaryEra StandardCrypto))
stringToValue input = calculateValue <$> fullParse input
 where
  fullParse :: String -> Either ParseError [PreValue]
  fullParse str = parse lexTokens "" str >>= parse preValueParser ""

calculateValue :: [PreValue] -> Value (MaryEra StandardCrypto)
calculateValue = mconcat . map preValtoValue

textToPolicyId :: Text -> PolicyID (MaryEra StandardCrypto)
textToPolicyId hashText =
  case hashFromStringAsHex $ Text.unpack hashText of
    Just h -> PolicyID $ ScriptHash h
    Nothing -> panic $ "PolicyId: " <> hashText <> " is not a hash."

preValtoValue :: PreValue -> Value (MaryEra StandardCrypto)
preValtoValue Addition = Value 0 mempty
preValtoValue (Lovelace w64) = Value (toInteger w64) mempty
preValtoValue (MultiAsset pId aId minted) =
  let pId' = textToPolicyId pId
      aId' = AssetID $ Text.encodeUtf8 aId
  in Value 0 $ Map.singleton pId' (Map.singleton aId' minted)

-- Parser

type TParser a = Parsec Tokens () a

data PreValue = Lovelace Word64
              | MultiAsset Text Text Integer
              -- ^ PolicyId AssetId AmountMinted
              | Addition
              deriving Show

preValueParser :: TParser [PreValue]
preValueParser = many1 (preValueLovelace <|> preValueMultiAsset <|> preValueAddition)

tokenToTParser :: (Token -> Maybe a) -> TParser a
tokenToTParser f =
  token
    (show . snd)
    fst
    $ \(_,t) -> f t

preValueLovelace :: TParser PreValue
preValueLovelace =
  tokenToTParser (\t -> case t of
                         LOVELACE n -> Just $ Lovelace n
                         _ -> Nothing
                 )

preValueMultiAsset :: TParser PreValue
preValueMultiAsset =
  tokenToTParser (\t -> case t of
                          MA pId aId aM -> Just $ MultiAsset pId aId aM
                          _ -> Nothing
                 )

preValueAddition :: TParser PreValue
preValueAddition =
  tokenToTParser (\t -> case t of
                          ADDITION -> Just Addition
                          _ -> Nothing
                 )

-- Lexer

type Tokens = [(SourcePos, Token)]

data Token = LOVELACE Word64
           | MA Text Text Integer
           -- ^ ScriptHash AssetId AmountMinted
           | ADDITION
           | PERIOD
           deriving (Eq, Ord, Show)

lexTokens :: Parser Tokens
lexTokens = spaces *> sepBy1 ((,) <$> getPosition <*> lexToken) spaces

lexToken :: Parser Token
lexToken =
      (lovelaceToken <?> "Expecting \"lovelace INT\"")
  <|> (addition <?> "Expecting \"+\"")
  <|> (valueToken <?> "Expecting \"hexidecimal.STRING INT\"")

-- Primitive Token Lexers

period :: Parser Token
period = PERIOD <$ string "."

word64 :: Parser Word64
word64 = do i <- integer
            if i > fromIntegral (maxBound :: Word64)
            then fail "Word64 max bound"
            else return $ fromInteger i

integer :: Parser Integer
integer = do d <- many1 digit
             notFollowedBy alphaNum
             return $ read d

lovelaceToken :: Parser Token
lovelaceToken = do
  _ <- string "lovelace"
  _ <- spaces
  w64 <- try word64 <?> "Word64"
  _ <- spaces
  return $ LOVELACE w64

valueToken :: Parser Token
valueToken =
      try valueTokenPidAndAssetId
  <|> try valueTokenFullySpecified
  <|> valueTokenPidOnly
  <* spaces

valueTokenFullySpecified :: Parser Token
valueTokenFullySpecified = do
  pId <- scriptHash
  _ <- period
  notFollowedBy space
  assetId <- try (manyTill (letter <|> digit) space)
  spaces
  i <- (try integer <?> "INT")
  let minted = fromInteger i
  return $ MA pId (Text.pack assetId) minted

valueTokenPidAndAssetId :: Parser Token
valueTokenPidAndAssetId = do
  pId <- scriptHash
  _ <- period
  notFollowedBy space
  assetId <- many (letter <|> digit)
  _ <- spaces <|> eof
  notFollowedBy integer
  return $ MA pId (Text.pack assetId) 1

valueTokenPidOnly :: Parser Token
valueTokenPidOnly = do
  pId <- scriptHash
  _ <- spaces
  i <- (try integer <?> "INT")
  let minted = fromInteger i
  return $ MA pId (Text.pack "") minted

scriptHash :: Parser Text
scriptHash = Text.pack <$> many1 hexDigit

addition :: Parser Token
addition = (ADDITION <$ string "+") <* spaces

