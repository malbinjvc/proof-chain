module Main (main) where

import Network.Wai.Handler.Warp (run)

import ProofChain.Types (newAppState)
import ProofChain.App (createApp)

main :: IO ()
main = do
  putStrLn "ProofChain starting on port 8080..."
  state <- newAppState
  application <- createApp state
  run 8080 application
