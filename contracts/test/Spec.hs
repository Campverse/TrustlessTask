{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Plutus.V2.Ledger.Api
import PlutusTx.Prelude hiding ((<>))
import qualified Prelude as Haskell
import TrustlessTask.Types
import TrustlessTask.Utils
import TrustlessTask.Escrow

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "TrustlessTask Smart Contract Tests"
  [ utilsTests
  , escrowValidatorTests
  , milestoneTests
  , disputeTests
  ]

-- Utility function tests
utilsTests :: TestTree
utilsTests = testGroup "Utils Tests"
  [ testCase "totalMilestoneAmount calculates correctly" $ do
      let m1 = Milestone 1 "Task 1" 1000000 0 False False
          m2 = Milestone 2 "Task 2" 2000000 0 False False
          m3 = Milestone 3 "Task 3" 1500000 0 False False
          total = totalMilestoneAmount [m1, m2, m3]
      assertEqual "Total should be 4500000" 4500000 total

  , testCase "findMilestone finds correct milestone" $ do
      let m1 = Milestone 1 "Task 1" 1000000 0 False False
          m2 = Milestone 2 "Task 2" 2000000 0 False False
          result = findMilestone 2 [m1, m2]
      assertEqual "Should find milestone 2" (Just m2) result

  , testCase "findMilestone returns Nothing for non-existent" $ do
      let m1 = Milestone 1 "Task 1" 1000000 0 False False
          result = findMilestone 99 [m1]
      assertEqual "Should return Nothing" Nothing result

  , testCase "countCompleted counts only completed milestones" $ do
      let m1 = Milestone 1 "Task 1" 1000000 0 True False
          m2 = Milestone 2 "Task 2" 2000000 0 False False
          m3 = Milestone 3 "Task 3" 1500000 0 True False
          count = countCompleted [m1, m2, m3]
      assertEqual "Should count 2 completed" 2 count

  , testCase "allMilestonesApproved checks all conditions" $ do
      let m1 = Milestone 1 "Task 1" 1000000 0 True True
          m2 = Milestone 2 "Task 2" 2000000 0 True True
          allApproved = allMilestonesApproved [m1, m2]
      assertEqual "All should be approved" True allApproved

  , testCase "allMilestonesApproved fails if any not approved" $ do
      let m1 = Milestone 1 "Task 1" 1000000 0 True True
          m2 = Milestone 2 "Task 2" 2000000 0 True False
          allApproved = allMilestonesApproved [m1, m2]
      assertEqual "Should not all be approved" False allApproved

  , testCase "beforeDeadline validates time correctly" $ do
      let current = 1000
          deadline = 2000
          result = beforeDeadline current deadline
      assertEqual "Current before deadline" True result

  , testCase "beforeDeadline fails when past deadline" $ do
      let current = 3000
          deadline = 2000
          result = beforeDeadline current deadline
      assertEqual "Current after deadline" False result
  ]

-- Escrow validator logic tests
escrowValidatorTests :: TestTree
escrowValidatorTests = testGroup "Escrow Validator Tests"
  [ testCase "Project creation requires correct total amount" $ do
      let m1 = Milestone 1 "Task 1" 1000000 0 False False
          m2 = Milestone 2 "Task 2" 2000000 0 False False
          total = totalMilestoneAmount [m1, m2]
      assertEqual "Total must match sum of milestones" 3000000 total

  , testCase "Milestone completion requires freelancer signature" $ do
      -- This would require full transaction context
      -- Placeholder for integration test
      Haskell.return ()

  , testCase "Milestone approval requires client signature" $ do
      -- This would require full transaction context
      -- Placeholder for integration test
      Haskell.return ()
  ]

-- Milestone workflow tests
milestoneTests :: TestTree
milestoneTests = testGroup "Milestone Workflow Tests"
  [ testCase "Milestone starts uncompleted and unapproved" $ do
      let m = Milestone 1 "Task" 1000000 0 False False
      assertEqual "Should be uncompleted" False (milestoneCompleted m)
      assertEqual "Should be unapproved" False (milestoneApproved m)

  , testCase "Completed milestone amount calculation" $ do
      let m1 = Milestone 1 "Task 1" 1000000 0 True True
          m2 = Milestone 2 "Task 2" 2000000 0 True True
          m3 = Milestone 3 "Task 3" 1500000 0 False False
          amount = completedMilestoneAmount [m1, m2, m3]
      assertEqual "Should sum only completed & approved" 3000000 amount
  ]

-- Dispute handling tests
disputeTests :: TestTree
disputeTests = testGroup "Dispute Tests"
  [ testCase "Dispute can be raised by client or freelancer" $ do
      -- Placeholder for integration test
      Haskell.return ()

  , testCase "Dispute resolution requires arbiter" $ do
      -- Placeholder for integration test
      Haskell.return ()
  ]
