{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module TrustlessTask.Utils where

import PlutusTx.Prelude
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import TrustlessTask.Types

-- | Check if transaction is signed by given public key hash
{-# INLINABLE signedBy #-}
signedBy :: TxInfo -> PubKeyHash -> Bool
signedBy txInfo pkh = txSignedBy txInfo pkh

-- | Get total value paid to a public key hash in transaction
{-# INLINABLE valuePaidTo #-}
valuePaidTo :: TxInfo -> PubKeyHash -> Value
valuePaidTo txInfo pkh = foldMap (txOutValue . txInInfoResolved) $
  filter (\txIn -> maybe False (== pkh) (toPubKeyHash $ txOutAddress $ txInInfoResolved txIn)) $
  txInfoInputs txInfo

-- | Extract public key hash from address
{-# INLINABLE toPubKeyHash #-}
toPubKeyHash :: Address -> Maybe PubKeyHash
toPubKeyHash (Address (PubKeyCredential pkh) _) = Just pkh
toPubKeyHash _ = Nothing

-- | Calculate total milestone amount
{-# INLINABLE totalMilestoneAmount #-}
totalMilestoneAmount :: [Milestone] -> Integer
totalMilestoneAmount = foldl (\acc m -> acc + milestoneAmount m) 0

-- | Find milestone by ID
{-# INLINABLE findMilestone #-}
findMilestone :: Integer -> [Milestone] -> Maybe Milestone
findMilestone mid [] = Nothing
findMilestone mid (m:ms)
  | milestoneId m == mid = Just m
  | otherwise = findMilestone mid ms

-- | Update milestone in list
{-# INLINABLE updateMilestone #-}
updateMilestone :: Milestone -> [Milestone] -> [Milestone]
updateMilestone updated = map (\m -> if milestoneId m == milestoneId updated then updated else m)

-- | Check if all milestones are completed and approved
{-# INLINABLE allMilestonesApproved #-}
allMilestonesApproved :: [Milestone] -> Bool
allMilestonesApproved = all (\m -> milestoneCompleted m && milestoneApproved m)

-- | Count completed milestones
{-# INLINABLE countCompleted #-}
countCompleted :: [Milestone] -> Integer
countCompleted = foldl (\acc m -> if milestoneCompleted m then acc + 1 else acc) 0

-- | Validate deadline hasn't passed
{-# INLINABLE beforeDeadline #-}
beforeDeadline :: POSIXTime -> POSIXTime -> Bool
beforeDeadline current deadline = current <= deadline

-- | Calculate amount for completed milestones
{-# INLINABLE completedMilestoneAmount #-}
completedMilestoneAmount :: [Milestone] -> Integer
completedMilestoneAmount = foldl (\acc m -> if milestoneCompleted m && milestoneApproved m 
                                             then acc + milestoneAmount m 
                                             else acc) 0
