{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TrustlessTask.Reputation where

import PlutusTx
import PlutusTx.Prelude
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import TrustlessTask.Types
import TrustlessTask.Utils

data ReputationRedeemer
  = UpdateReputation Integer Integer -- completed projects, rating
  | RecordDispute
  deriving stock (Haskell.Show)

PlutusTx.unstableMakeIsData ''ReputationRedeemer
PlutusTx.makeLift ''ReputationRedeemer

{-# INLINABLE mkReputationValidator #-}
mkReputationValidator :: ReputationDatum -> ReputationRedeemer -> ScriptContext -> Bool
mkReputationValidator datum redeemer ctx =
  case redeemer of
    UpdateReputation projects rating ->
      traceIfFalse "Invalid reputation update" (validateUpdate projects rating)

    RecordDispute ->
      traceIfFalse "Invalid dispute record" validateDisputeRecord
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Validate reputation update
    validateUpdate :: Integer -> Integer -> Bool
    validateUpdate projects rating =
      signedBy info (userPkh datum) &&
      projects >= completedProjects datum &&
      rating >= 0 && rating <= 500

    -- Validate dispute recording
    validateDisputeRecord :: Bool
    validateDisputeRecord =
      signedBy info (userPkh datum)

data ReputationTypes
instance Scripts.ValidatorTypes ReputationTypes where
  type instance DatumType ReputationTypes = ReputationDatum
  type instance RedeemerType ReputationTypes = ReputationRedeemer

reputationTypedValidator :: Scripts.TypedValidator ReputationTypes
reputationTypedValidator = Scripts.mkTypedValidator @ReputationTypes
  $$(PlutusTx.compile [|| mkReputationValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator

reputationValidator :: Validator
reputationValidator = Scripts.validatorScript reputationTypedValidator

reputationValidatorHash :: ValidatorHash
reputationValidatorHash = Scripts.validatorHash reputationTypedValidator
