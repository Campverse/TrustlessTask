{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Servant
import TrustlessTask.API
import TrustlessTask.API.Handlers

server :: Server TrustlessTaskAPI
server =
       createProjectHandler
  :<|> listProjectsHandler
  :<|> getProjectHandler
  :<|> completeMilestoneHandler
  :<|> approveMilestoneHandler
  :<|> cancelProjectHandler
  :<|> getUserProfileHandler
  :<|> createDisputeHandler
  :<|> getDisputeHandler
  :<|> resolveDisputeHandler

app :: Application
app = serve trustlessTaskAPI server

main :: IO ()
main = do
  putStrLn "Starting TrustlessTask API on port 8080..."
  run 8080 app
