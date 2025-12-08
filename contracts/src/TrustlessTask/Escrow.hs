{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TrustlessTask.Escrow where

import PlutusTx
import PlutusTx.Prelude
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import TrustlessTask.Types
import TrustlessTask.Utils

{-# INLINABLE mkEscrowValidator #-}
mkEscrowValidator :: ProjectDatum -> EscrowRedeemer -> ScriptContext -> Bool
mkEscrowValidator datum redeemer ctx =
  case redeemer of
    CreateProject ->
      traceIfFalse "Invalid project creation" validateCreation

    CompleteMilestone mid ->
      traceIfFalse "Milestone completion failed" (validateMilestoneCompletion mid)

    ApproveMilestone mid ->
      traceIfFalse "Milestone approval failed" (validateMilestoneApproval mid)

    ReleaseFunds mid ->
      traceIfFalse "Fund release failed" (validateFundRelease mid)

    CancelProject ->
      traceIfFalse "Project cancellation failed" validateCancellation

    RaiseDispute ->
      traceIfFalse "Dispute raising failed" validateDisputeRaise

    ResolveDispute outcome ->
      traceIfFalse "Dispute resolution failed" (validateDisputeResolution outcome)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Validate project creation
    validateCreation :: Bool
    validateCreation =
      signedBy info (client datum) &&
      projectStatus datum == Created &&
      totalMilestoneAmount (milestones datum) == totalAmount datum

    -- Validate milestone completion by freelancer
    validateMilestoneCompletion :: Integer -> Bool
    validateMilestoneCompletion mid =
      signedBy info (freelancer datum) &&
      projectStatus datum == InProgress &&
      case findMilestone mid (milestones datum) of
        Nothing -> False
        Just m -> not (milestoneCompleted m) &&
                  beforeDeadline (txInfoValidRange info `ivFrom`) (milestoneDeadline m)

    -- Validate milestone approval by client
    validateMilestoneApproval :: Integer -> Bool
    validateMilestoneApproval mid =
      signedBy info (client datum) &&
      case findMilestone mid (milestones datum) of
        Nothing -> False
        Just m -> milestoneCompleted m && not (milestoneApproved m)

    -- Validate fund release for approved milestone
    validateFundRelease :: Integer -> Bool
    validateFundRelease mid =
      case findMilestone mid (milestones datum) of
        Nothing -> False
        Just m ->
          milestoneCompleted m &&
          milestoneApproved m &&
          valuePaidTo info (freelancer datum) `geq` lovelaceValueOf (milestoneAmount m)

    -- Validate project cancellation
    validateCancellation :: Bool
    validateCancellation =
      (signedBy info (client datum) && countCompleted (milestones datum) == 0) ||
      (signedBy info (client datum) && signedBy info (freelancer datum))

    -- Validate dispute raising
    validateDisputeRaise :: Bool
    validateDisputeRaise =
      (signedBy info (client datum) || signedBy info (freelancer datum)) &&
      projectStatus datum /= Completed &&
      projectStatus datum /= Cancelled

    -- Validate dispute resolution by arbiter
    validateDisputeResolution :: Bool -> Bool
    validateDisputeResolution outcome =
      case arbiter datum of
        Nothing -> False
        Just arb ->
          signedBy info arb &&
          projectStatus datum == Disputed

    ivFrom :: Interval POSIXTime -> POSIXTime
    ivFrom (Interval (LowerBound (Finite t) _) _) = t
    ivFrom _ = 0

data EscrowTypes
instance Scripts.ValidatorTypes EscrowTypes where
  type instance DatumType EscrowTypes = ProjectDatum
  type instance RedeemerType EscrowTypes = EscrowRedeemer

escrowTypedValidator :: Scripts.TypedValidator EscrowTypes
escrowTypedValidator = Scripts.mkTypedValidator @EscrowTypes
  $$(PlutusTx.compile [|| mkEscrowValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator

escrowValidator :: Validator
escrowValidator = Scripts.validatorScript escrowTypedValidator

escrowValidatorHash :: ValidatorHash
escrowValidatorHash = Scripts.validatorHash escrowTypedValidator

escrowAddress :: Address
escrowAddress = scriptHashAddress escrowValidatorHash
