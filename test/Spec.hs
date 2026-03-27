{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Value(..), decode, encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai (Application)
import Network.Wai.Test (SResponse(..))
import Test.Hspec
import Test.Hspec.Wai

import ProofChain.App (createApp)
import ProofChain.Types
import ProofChain.Clients
import ProofChain.Services

main :: IO ()
main = hspec spec

-- Helper to make JSON POST requests
postJSON :: ByteString -> BL.ByteString -> WaiSession st SResponse
postJSON path = request methodPost path [(hContentType, "application/json")]

-- Helper for DELETE requests
deleteReq :: ByteString -> WaiSession st SResponse
deleteReq path = request methodDelete path [] ""

-- Helper to create a proposition via the API
createProp :: String -> String -> WaiSession st SResponse
createProp txt typ =
  postJSON "/api/propositions" $ encode $ object
    [ "text" .= txt
    , "type" .= typ
    ]

-- Fresh app for each test group
mkApp :: IO Application
mkApp = newAppState >>= createApp

spec :: Spec
spec = do
  -- Unit tests for Clients module (pure functions)
  describe "MockClaudeClient - validateProofChain" $ do
    it "returns invalid for empty propositions" $ do
      let result = validateProofChain []
      vrIsValid result `shouldBe` False

    it "returns invalid when no conclusion is present" $ do
      st <- newAppState
      p1 <- createProposition st "All cats are mammals" Premise
      let result = validateProofChain [p1]
      vrIsValid result `shouldBe` False

    it "validates when conclusion keywords match premises" $ do
      st <- newAppState
      p1 <- createProposition st "All cats are mammals" Premise
      p2 <- createProposition st "Fluffy is a cat" Premise
      c1 <- createProposition st "Fluffy is a cats mammals" Conclusion
      let result = validateProofChain [p1, p2, c1]
      vrIsValid result `shouldBe` True

    it "returns invalid when keywords do not match" $ do
      st <- newAppState
      p1 <- createProposition st "The sky is blue" Premise
      c1 <- createProposition st "Therefore economics is complex" Conclusion
      let result = validateProofChain [p1, c1]
      vrIsValid result `shouldBe` False

  describe "MockClaudeClient - checkFallacies" $ do
    it "detects ad hominem" $ do
      st <- newAppState
      p1 <- createProposition st "You are stupid so your argument is wrong" Premise
      let fallacies = checkFallacies [p1]
      length fallacies `shouldSatisfy` (> 0)
      fallacyName (head fallacies) `shouldBe` "Ad Hominem"

    it "detects straw man" $ do
      st <- newAppState
      p1 <- createProposition st "They claim that all regulation is bad" Premise
      let fallacies = checkFallacies [p1]
      length fallacies `shouldSatisfy` (> 0)
      fallacyName (head fallacies) `shouldBe` "Straw Man"

    it "detects circular reasoning" $ do
      st <- newAppState
      p1 <- createProposition st "Logic is important because reasoning matters" Premise
      c1 <- createProposition st "Reasoning matters because logic is important" Conclusion
      let fallacies = checkFallacies [p1, c1]
      let circular = filter (\f -> fallacyName f == "Circular Reasoning") fallacies
      length circular `shouldSatisfy` (> 0)

    it "returns empty for clean argument" $ do
      st <- newAppState
      p1 <- createProposition st "Water boils at 100 degrees Celsius" Premise
      c1 <- createProposition st "Steam is produced from boiling water" Conclusion
      let fallacies = checkFallacies [p1, c1]
      fallacies `shouldBe` []

    it "detects slippery slope" $ do
      st <- newAppState
      p1 <- createProposition st "This change will lead to total chaos" Premise
      let fallacies = checkFallacies [p1]
      length fallacies `shouldSatisfy` (> 0)

    it "detects appeal to authority" $ do
      st <- newAppState
      p1 <- createProposition st "Experts say this is the best approach" Premise
      let fallacies = checkFallacies [p1]
      length fallacies `shouldSatisfy` (> 0)
      fallacyName (head fallacies) `shouldBe` "Appeal to Authority"

    it "detects red herring" $ do
      st <- newAppState
      p1 <- createProposition st "But what about the other issue entirely" Premise
      let fallacies = checkFallacies [p1]
      length fallacies `shouldSatisfy` (> 0)

    it "detects false dilemma" $ do
      st <- newAppState
      p1 <- createProposition st "Either you agree with me or you are wrong" Premise
      let fallacies = checkFallacies [p1]
      length fallacies `shouldSatisfy` (> 0)

  describe "MockClaudeClient - evaluateArgument" $ do
    it "gives zero score for empty input" $ do
      let score = evaluateArgument []
      asScore score `shouldBe` 0.0

    it "gives higher score for more premises" $ do
      st <- newAppState
      p1 <- createProposition st "Premise one about dogs" Premise
      p2 <- createProposition st "Premise two about dogs" Premise
      p3 <- createProposition st "Premise three about dogs" Premise
      c1 <- createProposition st "Therefore dogs are great" Conclusion
      let score = evaluateArgument [p1, p2, p3, c1]
      asScore score `shouldSatisfy` (> 0.5)

    it "includes weaknesses when no conclusion" $ do
      st <- newAppState
      p1 <- createProposition st "Some premise" Premise
      let score = evaluateArgument [p1]
      asWeaknesses score `shouldSatisfy` (not . null)

  describe "MockClaudeClient - summarizeArgument" $ do
    it "summarizes empty argument" $ do
      let summary = summarizeArgument []
      asPremiseCount summary `shouldBe` 0

    it "summarizes a well-formed argument" $ do
      st <- newAppState
      p1 <- createProposition st "All birds can fly" Premise
      p2 <- createProposition st "Penguins are birds" Premise
      c1 <- createProposition st "Penguins can fly" Conclusion
      let summary = summarizeArgument [p1, p2, c1]
      asPremiseCount summary `shouldBe` 2

  -- Integration tests via WAI (hspec-wai)
  describe "Health endpoint" $ with mkApp $ do
    it "GET /health returns 200" $ do
      get "/health" `shouldRespondWith` 200

    it "GET /health returns service name" $ do
      resp <- get "/health"
      liftIO $ do
        let mVal = decode (simpleBody resp) :: Maybe Value
        case mVal of
          Just (Object obj) -> KM.lookup "service" obj `shouldBe` Just (String "proof-chain")
          _ -> expectationFailure "Expected JSON object"

  describe "Propositions CRUD" $ with mkApp $ do
    it "POST /api/propositions creates a proposition" $ do
      resp <- createProp "All humans are mortal" "premise"
      liftIO $ statusCode (simpleStatus resp) `shouldBe` 201
      liftIO $ do
        let mVal = decode (simpleBody resp) :: Maybe Value
        case mVal of
          Just (Object obj) ->
            KM.lookup "type" obj `shouldBe` Just (String "premise")
          _ -> expectationFailure "Expected JSON object"

    it "GET /api/propositions returns list" $ do
      _ <- createProp "Test premise" "premise"
      _ <- createProp "Test conclusion" "conclusion"
      resp <- get "/api/propositions"
      liftIO $ statusCode (simpleStatus resp) `shouldBe` 200
      liftIO $ do
        let mVal = decode (simpleBody resp) :: Maybe Value
        case mVal of
          Just (Array arr) -> length arr `shouldBe` 2
          _ -> expectationFailure "Expected JSON array"

    it "GET /api/propositions/:id returns specific proposition" $ do
      _ <- createProp "Specific prop" "hypothesis"
      resp <- get "/api/propositions/1"
      liftIO $ statusCode (simpleStatus resp) `shouldBe` 200

    it "GET /api/propositions/:id returns 404 for missing" $ do
      get "/api/propositions/999" `shouldRespondWith` 404

    it "DELETE /api/propositions/:id removes proposition" $ do
      _ <- createProp "To be deleted" "premise"
      resp <- deleteReq "/api/propositions/1"
      liftIO $ statusCode (simpleStatus resp) `shouldBe` 200
      get "/api/propositions/1" `shouldRespondWith` 404

    it "DELETE /api/propositions/:id returns 404 for missing" $ do
      resp <- deleteReq "/api/propositions/999"
      liftIO $ statusCode (simpleStatus resp) `shouldBe` 404

  describe "Proof validation" $ with mkApp $ do
    it "POST /api/proofs/validate validates a proof chain" $ do
      _ <- createProp "All mammals breathe air" "premise"
      _ <- createProp "Dogs are mammals" "premise"
      _ <- createProp "Dogs breathe mammals air" "conclusion"
      resp <- postJSON "/api/proofs/validate" $ encode $ object
        [ "propositionIds" .= ([1, 2, 3] :: [Int]) ]
      liftIO $ statusCode (simpleStatus resp) `shouldBe` 200

    it "POST /api/proofs/validate returns 404 for missing propositions" $ do
      resp <- postJSON "/api/proofs/validate" $ encode $ object
        [ "propositionIds" .= ([99, 100] :: [Int]) ]
      liftIO $ statusCode (simpleStatus resp) `shouldBe` 404

    it "POST /api/proofs/validate returns 400 for bad body" $ do
      resp <- postJSON "/api/proofs/validate" "{invalid json"
      liftIO $ statusCode (simpleStatus resp) `shouldBe` 400

  describe "Fallacy checking" $ with mkApp $ do
    it "POST /api/fallacies/check finds fallacies" $ do
      _ <- createProp "You are an idiot so you cannot be right" "premise"
      resp <- postJSON "/api/fallacies/check" $ encode $ object
        [ "propositionIds" .= ([1] :: [Int]) ]
      liftIO $ statusCode (simpleStatus resp) `shouldBe` 200
      liftIO $ do
        let mVal = decode (simpleBody resp) :: Maybe Value
        case mVal of
          Just (Object obj) ->
            case KM.lookup "count" obj of
              Just (Number n) -> n `shouldSatisfy` (> 0)
              _ -> expectationFailure "Expected count field"
          _ -> expectationFailure "Expected JSON object"

    it "POST /api/fallacies/check returns empty for clean argument" $ do
      _ <- createProp "Water boils at 100 degrees" "premise"
      resp <- postJSON "/api/fallacies/check" $ encode $ object
        [ "propositionIds" .= ([1] :: [Int]) ]
      liftIO $ statusCode (simpleStatus resp) `shouldBe` 200
      liftIO $ do
        let mVal = decode (simpleBody resp) :: Maybe Value
        case mVal of
          Just (Object obj) ->
            case KM.lookup "count" obj of
              Just (Number n) -> n `shouldBe` 0
              _ -> expectationFailure "Expected count field"
          _ -> expectationFailure "Expected JSON object"

  describe "Argument evaluation" $ with mkApp $ do
    it "POST /api/arguments/evaluate returns score" $ do
      _ <- createProp "Exercise improves health" "premise"
      _ <- createProp "Regular exercise reduces risk" "premise"
      _ <- createProp "People should exercise regularly" "conclusion"
      resp <- postJSON "/api/arguments/evaluate" $ encode $ object
        [ "propositionIds" .= ([1, 2, 3] :: [Int]) ]
      liftIO $ statusCode (simpleStatus resp) `shouldBe` 200
      liftIO $ do
        let mVal = decode (simpleBody resp) :: Maybe Value
        case mVal of
          Just (Object obj) ->
            case KM.lookup "score" obj of
              Just (Number n) -> n `shouldSatisfy` (> 0)
              _ -> expectationFailure "Expected score field"
          _ -> expectationFailure "Expected JSON object"

  describe "Argument summarization" $ with mkApp $ do
    it "POST /api/arguments/summarize returns summary" $ do
      _ <- createProp "The earth orbits the sun" "premise"
      _ <- createProp "One orbit takes one year" "premise"
      _ <- createProp "Therefore a year is one orbit" "conclusion"
      resp <- postJSON "/api/arguments/summarize" $ encode $ object
        [ "propositionIds" .= ([1, 2, 3] :: [Int]) ]
      liftIO $ statusCode (simpleStatus resp) `shouldBe` 200
      liftIO $ do
        let mVal = decode (simpleBody resp) :: Maybe Value
        case mVal of
          Just (Object obj) ->
            KM.lookup "premiseCount" obj `shouldBe` Just (Number 2)
          _ -> expectationFailure "Expected JSON object"

  describe "Stats endpoint" $ with mkApp $ do
    it "GET /api/stats returns initial stats" $ do
      resp <- get "/api/stats"
      liftIO $ statusCode (simpleStatus resp) `shouldBe` 200
      liftIO $ do
        let mVal = decode (simpleBody resp) :: Maybe Value
        case mVal of
          Just (Object obj) -> do
            KM.lookup "totalPropositions" obj `shouldBe` Just (Number 0)
            KM.lookup "proofsValidated" obj `shouldBe` Just (Number 0)
          _ -> expectationFailure "Expected JSON object"

    it "GET /api/stats reflects created propositions" $ do
      _ <- createProp "A test proposition" "premise"
      resp <- get "/api/stats"
      liftIO $ do
        let mVal = decode (simpleBody resp) :: Maybe Value
        case mVal of
          Just (Object obj) ->
            KM.lookup "totalPropositions" obj `shouldBe` Just (Number 1)
          _ -> expectationFailure "Expected JSON object"

  describe "Error handling" $ with mkApp $ do
    it "POST /api/propositions rejects invalid type" $ do
      resp <- postJSON "/api/propositions" $ encode $ object
        [ "text" .= ("test" :: String)
        , "type" .= ("invalid_type" :: String)
        ]
      liftIO $ statusCode (simpleStatus resp) `shouldBe` 400

    it "POST /api/propositions rejects missing fields" $ do
      resp <- postJSON "/api/propositions" $ encode $ object
        [ "text" .= ("test" :: String) ]
      liftIO $ statusCode (simpleStatus resp) `shouldBe` 400

    it "POST /api/fallacies/check returns 404 for missing props" $ do
      resp <- postJSON "/api/fallacies/check" $ encode $ object
        [ "propositionIds" .= ([999] :: [Int]) ]
      liftIO $ statusCode (simpleStatus resp) `shouldBe` 404

    it "POST /api/arguments/evaluate returns 404 for missing props" $ do
      resp <- postJSON "/api/arguments/evaluate" $ encode $ object
        [ "propositionIds" .= ([999] :: [Int]) ]
      liftIO $ statusCode (simpleStatus resp) `shouldBe` 404

    it "POST /api/arguments/summarize returns 404 for missing props" $ do
      resp <- postJSON "/api/arguments/summarize" $ encode $ object
        [ "propositionIds" .= ([999] :: [Int]) ]
      liftIO $ statusCode (simpleStatus resp) `shouldBe` 404
