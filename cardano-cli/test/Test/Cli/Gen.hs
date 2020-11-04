{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cli.Gen where

import           Cardano.Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as Text

import           Cardano.CLI.Helpers (textShow)
import           Cardano.CLI.Mary.Parser (Token (..), textToPolicyId)
import           Cardano.Crypto.Hash (hashToTextAsHex)
import           Cardano.Ledger.Mary (MaryEra)
import           Cardano.Ledger.Mary.Value (AssetID (..), Value (..))
import           Cardano.Ledger.ShelleyMA.Timelocks (Timelock (TimelockAnd), hashTimelockScript)
import qualified Data.Sequence.Strict as Strict
import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Ouroboros.Consensus.Shelley.Eras (StandardCrypto)
import qualified Shelley.Spec.Ledger.Scripts as Shelley

-- Lexing Token Generators

genVariableSpace :: Gen Text
genVariableSpace = Gen.text (Range.constant 1 10) $ return ' '

genLovelaceToken :: Gen (Text, Token)
genLovelaceToken = do w64 <- Gen.word64 Range.constantBounded
                      space1 <- genVariableSpace
                      space2 <- genVariableSpace
                      return ("lovelace" <> space1 <> textShow w64 <> space2, LOVELACE w64)

genValueTokenFullySpecified :: Gen (Text, Token)
genValueTokenFullySpecified = do
  sHash <- genScriptHashMaryText
  assetId <- Gen.text (Range.constant 1 15) Gen.alphaNum
  let mBound = fromIntegral (maxBound :: Word64)
  minted <- Gen.integral_ (Range.constant 1 mBound)
  let mintedText = textShow minted
  variableSpace <- genVariableSpace
  return ( sHash <> "." <> assetId <> variableSpace <> mintedText
         , MA sHash assetId minted
         )

genValueTokenPidAndAssetId :: Gen (Text, Token)
genValueTokenPidAndAssetId = do
  sHash <- genScriptHashMaryText
  assetId <- Gen.text (Range.constant 1 15) Gen.alphaNum
  return ( sHash <> "." <> assetId
         , MA sHash assetId 1
         )

genValueTokenPidOnly :: Gen (Text, Token)
genValueTokenPidOnly = do
  sHash <- genScriptHashMaryText
  let mBound = fromIntegral (maxBound :: Word64)
  minted <- Gen.integral_ (Range.constant 1 mBound)
  let mintedText = textShow minted
  variableSpace <- genVariableSpace
  return ( sHash <> variableSpace <> mintedText
         , MA sHash "" minted
         )

genValueTokens :: Gen [(Text, Token)]
genValueTokens = do
 valsFulSpec <- Gen.list (Range.constant 1 10) genValueTokenFullySpecified
 valsPidAssetId <- Gen.list (Range.constant 1 10) genValueTokenPidAndAssetId
 valsPidOnly <- Gen.list (Range.constant 1 10) genValueTokenPidOnly
 return  $ valsFulSpec ++ valsPidAssetId ++ valsPidOnly

genAdditionToken :: Gen (Text, Token)
genAdditionToken = do spaces1 <- genVariableSpace
                      return ("+" <> spaces1, ADDITION)

genTokens :: Gen (Text, [Token])
genTokens = do lovelaces <- Gen.list (Range.constant 1 10) genLovelaceToken
               vals <- genValueTokens
               let total = lovelaces ++ vals
               additionTk <- genAdditionToken
               return . sequence $ intersperse additionTk total

-- Parsing Token Generators

genValues :: Gen (Text, Value (MaryEra StandardCrypto))
genValues = do lovelaces <- Gen.list (Range.constant 1 10) genLovelaceValue
               vals <- Gen.list (Range.constant 1 10) genMultiAssetValue
               add <- genAdditionValue
               let total = lovelaces ++ vals
               return . mconcat  $ intersperse add total

genLovelaceValue :: Gen (Text, Value (MaryEra StandardCrypto))
genLovelaceValue = do (input, LOVELACE w64) <- genLovelaceToken
                      return (input, Value (toInteger w64) mempty)

genMultiAssetValue :: Gen (Text, Value (MaryEra StandardCrypto))
genMultiAssetValue = do vTkns <- genValueTokens
                        (input, MA scriptH assetId minted) <- Gen.element vTkns
                        let pId' = textToPolicyId scriptH
                            aId' = AssetID $ Text.encodeUtf8 assetId
                        return ( input
                               , Value 0 (Map.singleton pId' (Map.singleton aId' minted))
                               )

genAdditionValue :: Gen (Text, Value (MaryEra StandardCrypto))
genAdditionValue = do (input, ADDITION) <- genAdditionToken
                      return (input, Value 0 mempty)

genMaryScriptHash :: Gen (Shelley.ScriptHash (MaryEra StandardCrypto))
genMaryScriptHash = return . hashTimelockScript $ TimelockAnd Strict.empty

genScriptHashMaryText :: Gen Text
genScriptHashMaryText = do Shelley.ScriptHash h <- genMaryScriptHash
                           return $ hashToTextAsHex h

