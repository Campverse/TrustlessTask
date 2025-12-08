{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import Codec.Serialise (serialise)
import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import qualified Plutus.V2.Ledger.Api as Plutus
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)

import TrustlessTask.Escrow
import TrustlessTask.Dispute
import TrustlessTask.Reputation

-- | Write Plutus scripts to files
writeScriptToFile :: FilePath -> Plutus.Validator -> IO ()
writeScriptToFile path validator = do
  let script = PlutusScriptSerialised $ SBS.toShort $ LBS.toStrict $ serialise validator
  result <- writeFileTextEnvelope path Nothing script
  case result of
    Left err -> putStrLn $ "Error writing script: " ++ show err
    Right () -> putStrLn $ "Successfully wrote script to " ++ path

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["write-scripts", "--out-dir", outDir] -> do
      createDirectoryIfMissing True outDir
      
      putStrLn "Writing Plutus scripts..."
      
      writeScriptToFile (outDir ++ "/escrow.plutus") escrowValidator
      writeScriptToFile (outDir ++ "/dispute.plutus") disputeValidator
      writeScriptToFile (outDir ++ "/reputation.plutus") reputationValidator
      
      putStrLn "All scripts written successfully!"
      
    _ -> putStrLn "Usage: trustless-task-contracts write-scripts --out-dir <directory>"
