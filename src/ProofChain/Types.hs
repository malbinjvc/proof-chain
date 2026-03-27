{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ProofChain.Types
  ( Proposition(..)
  , PropositionType(..)
  , ProofChain(..)
  , Fallacy(..)
  , FallacySeverity(..)
  , ValidationResult(..)
  , ArgumentScore(..)
  , ArgumentSummary(..)
  , AppState(..)
  , Stats(..)
  , CreatePropositionReq(..)
  , ValidateProofReq(..)
  , CheckFallacyReq(..)
  , EvaluateArgumentReq(..)
  , SummarizeReq(..)
  , ErrorResponse(..)
  , newAppState
  ) where

import Data.Aeson
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Proposition type classification
data PropositionType
  = Premise
  | Conclusion
  | Hypothesis
  deriving (Show, Eq, Generic)

instance ToJSON PropositionType where
  toJSON Premise    = String "premise"
  toJSON Conclusion = String "conclusion"
  toJSON Hypothesis = String "hypothesis"

instance FromJSON PropositionType where
  parseJSON = withText "PropositionType" $ \t ->
    case t of
      "premise"    -> pure Premise
      "conclusion" -> pure Conclusion
      "hypothesis" -> pure Hypothesis
      _            -> fail "Invalid proposition type: expected premise, conclusion, or hypothesis"

-- | A logical proposition
data Proposition = Proposition
  { propId        :: Int
  , propText      :: Text
  , propType      :: PropositionType
  , propCreatedAt :: UTCTime
  } deriving (Show, Eq, Generic)

instance ToJSON Proposition where
  toJSON Proposition{..} = object
    [ "id"        .= propId
    , "text"      .= propText
    , "type"      .= propType
    , "createdAt" .= propCreatedAt
    ]

instance FromJSON Proposition where
  parseJSON = withObject "Proposition" $ \v -> Proposition
    <$> v .: "id"
    <*> v .: "text"
    <*> v .: "type"
    <*> v .: "createdAt"

-- | Validation result from AI analysis
data ValidationResult = ValidationResult
  { vrIsValid   :: Bool
  , vrReasoning :: Text
  , vrDetails   :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON ValidationResult where
  toJSON ValidationResult{..} = object
    [ "isValid"   .= vrIsValid
    , "reasoning" .= vrReasoning
    , "details"   .= vrDetails
    ]

instance FromJSON ValidationResult where
  parseJSON = withObject "ValidationResult" $ \v -> ValidationResult
    <$> v .: "isValid"
    <*> v .: "reasoning"
    <*> v .: "details"

-- | A proof chain linking propositions
data ProofChain = ProofChainData
  { pcId               :: Int
  , pcPropositionIds   :: [Int]
  , pcIsValid          :: Bool
  , pcValidationResult :: ValidationResult
  } deriving (Show, Eq, Generic)

instance ToJSON ProofChain where
  toJSON ProofChainData{..} = object
    [ "id"               .= pcId
    , "propositionIds"   .= pcPropositionIds
    , "isValid"          .= pcIsValid
    , "validationResult" .= pcValidationResult
    ]

-- | Severity levels for fallacies
data FallacySeverity
  = Low
  | Medium
  | High
  | Critical
  deriving (Show, Eq, Generic)

instance ToJSON FallacySeverity where
  toJSON Low      = String "low"
  toJSON Medium   = String "medium"
  toJSON High     = String "high"
  toJSON Critical = String "critical"

instance FromJSON FallacySeverity where
  parseJSON = withText "FallacySeverity" $ \t ->
    case t of
      "low"      -> pure Low
      "medium"   -> pure Medium
      "high"     -> pure High
      "critical" -> pure Critical
      _          -> fail "Invalid severity"

-- | A detected logical fallacy
data Fallacy = Fallacy
  { fallacyId          :: Int
  , fallacyName        :: Text
  , fallacyDescription :: Text
  , fallacySeverity    :: FallacySeverity
  , fallacyLocation    :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Fallacy where
  toJSON Fallacy{..} = object
    [ "id"          .= fallacyId
    , "name"        .= fallacyName
    , "description" .= fallacyDescription
    , "severity"    .= fallacySeverity
    , "location"    .= fallacyLocation
    ]

-- | Argument strength score
data ArgumentScore = ArgumentScore
  { asScore     :: Double
  , asReasoning :: Text
  , asStrengths :: [Text]
  , asWeaknesses :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON ArgumentScore where
  toJSON ArgumentScore{..} = object
    [ "score"      .= asScore
    , "reasoning"  .= asReasoning
    , "strengths"  .= asStrengths
    , "weaknesses" .= asWeaknesses
    ]

-- | Argument summary
data ArgumentSummary = ArgumentSummary
  { asSummaryText    :: Text
  , asPremiseCount   :: Int
  , asConclusionText :: Text
  , asLogicalFlow    :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON ArgumentSummary where
  toJSON ArgumentSummary{..} = object
    [ "summary"       .= asSummaryText
    , "premiseCount"  .= asPremiseCount
    , "conclusion"    .= asConclusionText
    , "logicalFlow"   .= asLogicalFlow
    ]

-- | Application statistics
data Stats = Stats
  { statsTotalPropositions :: Int
  , statsProofsValidated   :: Int
  , statsFallaciesFound    :: Int
  , statsArgumentsEvaluated :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON Stats where
  toJSON Stats{..} = object
    [ "totalPropositions"  .= statsTotalPropositions
    , "proofsValidated"    .= statsProofsValidated
    , "fallaciesFound"     .= statsFallaciesFound
    , "argumentsEvaluated" .= statsArgumentsEvaluated
    ]

-- | Mutable application state
data AppState = AppState
  { statePropositions      :: IORef (Map Int Proposition)
  , stateNextPropId        :: IORef Int
  , stateNextProofId       :: IORef Int
  , stateProofsValidated   :: IORef Int
  , stateFallaciesFound    :: IORef Int
  , stateArgsEvaluated     :: IORef Int
  }

-- | Create fresh application state
newAppState :: IO AppState
newAppState = AppState
  <$> newIORef Map.empty
  <*> newIORef 1
  <*> newIORef 1
  <*> newIORef 0
  <*> newIORef 0
  <*> newIORef 0

-- Request types

data CreatePropositionReq = CreatePropositionReq
  { cprText :: Text
  , cprType :: PropositionType
  } deriving (Show, Generic)

instance FromJSON CreatePropositionReq where
  parseJSON = withObject "CreatePropositionReq" $ \v -> CreatePropositionReq
    <$> v .: "text"
    <*> v .: "type"

instance ToJSON CreatePropositionReq where
  toJSON CreatePropositionReq{..} = object
    [ "text" .= cprText
    , "type" .= cprType
    ]

data ValidateProofReq = ValidateProofReq
  { vprPropositionIds :: [Int]
  } deriving (Show, Generic)

instance FromJSON ValidateProofReq where
  parseJSON = withObject "ValidateProofReq" $ \v -> ValidateProofReq
    <$> v .: "propositionIds"

data CheckFallacyReq = CheckFallacyReq
  { cfrPropositionIds :: [Int]
  } deriving (Show, Generic)

instance FromJSON CheckFallacyReq where
  parseJSON = withObject "CheckFallacyReq" $ \v -> CheckFallacyReq
    <$> v .: "propositionIds"

data EvaluateArgumentReq = EvaluateArgumentReq
  { earPropositionIds :: [Int]
  } deriving (Show, Generic)

instance FromJSON EvaluateArgumentReq where
  parseJSON = withObject "EvaluateArgumentReq" $ \v -> EvaluateArgumentReq
    <$> v .: "propositionIds"

data SummarizeReq = SummarizeReq
  { srPropositionIds :: [Int]
  } deriving (Show, Generic)

instance FromJSON SummarizeReq where
  parseJSON = withObject "SummarizeReq" $ \v -> SummarizeReq
    <$> v .: "propositionIds"

-- Error response

data ErrorResponse = ErrorResponse
  { errMessage :: Text
  } deriving (Show, Generic)

instance ToJSON ErrorResponse where
  toJSON ErrorResponse{..} = object
    [ "error" .= errMessage
    ]
