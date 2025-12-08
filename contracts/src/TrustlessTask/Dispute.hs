{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TrustlessTask.Dispute where

import PlutusTx
import PlutusTx.Prelude
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import TrustlessTask.Types
import TrustlessTask.Utils

data DisputeRedeemer
  = CreateDispute
  | ResolveDisputeAction Bool
  deriving stock (Haskell.Show)

PlutusTx.unstableMakeIsData ''DisputeRedeemer
PlutusTx.makeLift ''DisputeRedeemer

{-# INLINABLE mkDisputeValidator #-}
mkDisputeValidator :: DisputeDatum -> DisputeRedeemer -> ScriptContext -> Bool
mkDisputeValidator datum redeemer ctx =
  case redeemer of
    CreateDispute ->
      traceIfFalse "Invalid dispute creation" validateDisputeCreation

    ResolveDisputeAction outcome ->
      traceIfFalse "Invalid dispute resolution" (validateResolution outcome)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Validate dispute creation
    validateDisputeCreation :: Bool
    validateDisputeCreation =
      signedBy info (disputeRaiser datum) &&
      not (disputeResolved datum)

    -- Validate dispute resolution by arbiter
    validateResolution :: Bool -> Bool
    validateResolution outcome =
      signedBy info (disputeArbiter datum) &&
      not (disputeResolved datum)

data DisputeTypes
instance Scripts.ValidatorTypes DisputeTypes where
  type instance DatumType DisputeTypes = DisputeDatum
  type instance RedeemerType DisputeTypes = DisputeRedeemer

disputeTypedValidator :: Scripts.TypedValidator DisputeTypes
disputeTypedValidator = Scripts.mkTypedValidator @DisputeTypes
  $$(PlutusTx.compile [|| mkDisputeValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator

disputeValidator :: Validator
disputeValidator = Scripts.validatorScript disputeTypedValidator

disputeValidatorHash :: ValidatorHash
disputeValidatorHash = Scripts.validatorHash disputeTypedValidator
