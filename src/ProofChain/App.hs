{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ProofChain.App
  ( createApp
  , app
  ) where

import Data.Aeson (encode, eitherDecode, object, (.=), ToJSON)
import Data.IORef (readIORef, modifyIORef')
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Network.HTTP.Types
import Network.Wai

import ProofChain.Types
import ProofChain.Services
import ProofChain.Clients

-- | Create the WAI application with given state
createApp :: AppState -> IO Application
createApp state = return (app state)

-- | The WAI application - route by method and path
app :: AppState -> Application
app state req respond =
  let method = requestMethod req
      path   = pathInfo req
  in case (method, path) of
    ("GET",    ["health"])                        -> healthHandler respond
    ("POST",   ["api", "propositions"])           -> createPropHandler state req respond
    ("GET",    ["api", "propositions"])            -> listPropHandler state respond
    ("GET",    ["api", "propositions", pid])       -> getPropHandler state pid respond
    ("DELETE", ["api", "propositions", pid])       -> deletePropHandler state pid respond
    ("POST",   ["api", "proofs", "validate"])     -> validateHandler state req respond
    ("POST",   ["api", "fallacies", "check"])     -> checkFallaciesHandler state req respond
    ("POST",   ["api", "arguments", "evaluate"])  -> evaluateHandler state req respond
    ("POST",   ["api", "arguments", "summarize"]) -> summarizeHandler state req respond
    ("GET",    ["api", "stats"])                  -> statsHandler state respond
    _ -> respond $ jsonResp status404 (ErrorResponse "Not found")

-- | JSON response helper
jsonResp :: ToJSON a => Status -> a -> Response
jsonResp s val = responseLBS s [(hContentType, "application/json")] (encode val)

-- | Parse text to Int
parseId :: T.Text -> Maybe Int
parseId t = case TR.decimal t of
  Right (n, rest) | T.null rest -> Just n
  _ -> Nothing

-- Handlers

healthHandler :: (Response -> IO ResponseReceived) -> IO ResponseReceived
healthHandler respond =
  respond $ jsonResp status200 $ object
    [ "status"  .= ("ok" :: T.Text)
    , "service" .= ("proof-chain" :: T.Text)
    ]

createPropHandler :: AppState -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
createPropHandler state req respond = do
  bodyBytes <- strictRequestBody req
  case eitherDecode bodyBytes of
    Left err ->
      respond $ jsonResp status400 $ ErrorResponse (T.pack $ "Invalid request body: " ++ err)
    Right r -> do
      prop <- createProposition state (cprText r) (cprType r)
      respond $ jsonResp status201 prop

listPropHandler :: AppState -> (Response -> IO ResponseReceived) -> IO ResponseReceived
listPropHandler state respond = do
  props <- listPropositions state
  respond $ jsonResp status200 props

getPropHandler :: AppState -> T.Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
getPropHandler state pidText respond =
  case parseId pidText of
    Nothing -> respond $ jsonResp status404 $ ErrorResponse "Invalid ID"
    Just pid -> do
      mProp <- getProposition state pid
      case mProp of
        Just p  -> respond $ jsonResp status200 p
        Nothing -> respond $ jsonResp status404 $ ErrorResponse "Proposition not found"

deletePropHandler :: AppState -> T.Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
deletePropHandler state pidText respond =
  case parseId pidText of
    Nothing -> respond $ jsonResp status404 $ ErrorResponse "Invalid ID"
    Just pid -> do
      deleted <- deleteProposition state pid
      if deleted
        then respond $ jsonResp status200 $ object ["message" .= ("Proposition deleted" :: T.Text)]
        else respond $ jsonResp status404 $ ErrorResponse "Proposition not found"

validateHandler :: AppState -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
validateHandler state req respond = do
  bodyBytes <- strictRequestBody req
  case eitherDecode bodyBytes of
    Left err ->
      respond $ jsonResp status400 $ ErrorResponse (T.pack $ "Invalid request body: " ++ err)
    Right r -> do
      propsResult <- lookupPropositions state (vprPropositionIds r)
      case propsResult of
        Left errMsg ->
          respond $ jsonResp status404 $ ErrorResponse errMsg
        Right props -> do
          let validation = validateProofChain props
          proofId <- readProofId state
          let proof = ProofChainData proofId (vprPropositionIds r) (vrIsValid validation) validation
          incrementNextProofId state
          incrementProofsValidated state
          respond $ jsonResp status200 proof

checkFallaciesHandler :: AppState -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
checkFallaciesHandler state req respond = do
  bodyBytes <- strictRequestBody req
  case eitherDecode bodyBytes of
    Left err ->
      respond $ jsonResp status400 $ ErrorResponse (T.pack $ "Invalid request body: " ++ err)
    Right r -> do
      propsResult <- lookupPropositions state (cfrPropositionIds r)
      case propsResult of
        Left errMsg ->
          respond $ jsonResp status404 $ ErrorResponse errMsg
        Right props -> do
          let fallacies = numberFallacies $ checkFallacies props
          incrementFallaciesFound state (length fallacies)
          respond $ jsonResp status200 $ object
            [ "fallacies" .= fallacies
            , "count"     .= length fallacies
            ]

evaluateHandler :: AppState -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
evaluateHandler state req respond = do
  bodyBytes <- strictRequestBody req
  case eitherDecode bodyBytes of
    Left err ->
      respond $ jsonResp status400 $ ErrorResponse (T.pack $ "Invalid request body: " ++ err)
    Right r -> do
      propsResult <- lookupPropositions state (earPropositionIds r)
      case propsResult of
        Left errMsg ->
          respond $ jsonResp status404 $ ErrorResponse errMsg
        Right props -> do
          let score = evaluateArgument props
          incrementArgsEvaluated state
          respond $ jsonResp status200 score

summarizeHandler :: AppState -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
summarizeHandler state req respond = do
  bodyBytes <- strictRequestBody req
  case eitherDecode bodyBytes of
    Left err ->
      respond $ jsonResp status400 $ ErrorResponse (T.pack $ "Invalid request body: " ++ err)
    Right r -> do
      propsResult <- lookupPropositions state (srPropositionIds r)
      case propsResult of
        Left errMsg ->
          respond $ jsonResp status404 $ ErrorResponse errMsg
        Right props ->
          respond $ jsonResp status200 $ summarizeArgument props

statsHandler :: AppState -> (Response -> IO ResponseReceived) -> IO ResponseReceived
statsHandler state respond = do
  stats <- getStats state
  respond $ jsonResp status200 stats

-- Helper: read current proof ID
readProofId :: AppState -> IO Int
readProofId state = readIORef (stateNextProofId state)

-- Helper: increment proof ID
incrementNextProofId :: AppState -> IO ()
incrementNextProofId state = modifyIORef' (stateNextProofId state) (+ 1)

-- Helper: number fallacies sequentially
numberFallacies :: [Fallacy] -> [Fallacy]
numberFallacies = zipWith (\i f -> f { fallacyId = i }) [1..]
