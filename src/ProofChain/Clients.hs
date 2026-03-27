{-# LANGUAGE OverloadedStrings #-}

module ProofChain.Clients
  ( validateProofChain
  , checkFallacies
  , evaluateArgument
  , summarizeArgument
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import ProofChain.Types

-- | Mock Claude AI Client
-- Validates proof chains deterministically by checking keyword overlap

-- | Validate a proof chain: checks if conclusion keywords appear in premises
validateProofChain :: [Proposition] -> ValidationResult
validateProofChain props
  | null props = ValidationResult False "No propositions provided" []
  | null conclusions = ValidationResult False "No conclusion found in the proof chain" details
  | otherwise = ValidationResult valid reasoning details
  where
    premises    = filter (\p -> propType p == Premise) props
    conclusions = filter (\p -> propType p == Conclusion) props
    hypotheses  = filter (\p -> propType p == Hypothesis) props

    premiseWords    = concatMap (getKeywords . propText) premises
    conclusionWords = concatMap (getKeywords . propText) conclusions

    -- Check how many conclusion keywords appear in premises
    matchingWords = filter (`elem` premiseWords) conclusionWords
    matchRatio
      | null conclusionWords = 0.0
      | otherwise = fromIntegral (length matchingWords) / fromIntegral (length conclusionWords) :: Double

    valid = matchRatio >= 0.3 && not (null premises)

    reasoning
      | valid     = "The conclusion follows from the premises with sufficient keyword overlap (" <> tshow (round' matchRatio) <> "% match)"
      | null premises = "No premises provided to support the conclusion"
      | otherwise = "Insufficient logical connection between premises and conclusion (" <> tshow (round' matchRatio) <> "% keyword match)"

    details =
      [ "Premises: " <> tshow (length premises)
      , "Conclusions: " <> tshow (length conclusions)
      , "Hypotheses: " <> tshow (length hypotheses)
      , "Keyword match ratio: " <> tshow (round' matchRatio) <> "%"
      ]

-- | Check for common logical fallacies in propositions
checkFallacies :: [Proposition] -> [Fallacy]
checkFallacies props = concat
  [ checkAdHominem props
  , checkStrawMan props
  , checkCircularReasoning props
  , checkFalseDisjunction props
  , checkAppealToAuthority props
  , checkSlipperySlope props
  , checkRedHerring props
  ]

checkAdHominem :: [Proposition] -> [Fallacy]
checkAdHominem props =
  [ Fallacy 0 "Ad Hominem" desc High (propText p)
  | p <- props
  , let txt = T.toLower (propText p)
  , any (`T.isInfixOf` txt) adHominemPatterns
  , let desc = "Attack on the person rather than the argument: \"" <> propText p <> "\""
  ]
  where
    adHominemPatterns = ["stupid", "idiot", "you're wrong because you", "can't trust", "what do you know", "ignorant"]

checkStrawMan :: [Proposition] -> [Fallacy]
checkStrawMan props =
  [ Fallacy 0 "Straw Man" desc Medium (propText p)
  | p <- props
  , let txt = T.toLower (propText p)
  , any (`T.isInfixOf` txt) strawManPatterns
  , let desc = "Misrepresentation of opposing argument: \"" <> propText p <> "\""
  ]
  where
    strawManPatterns = ["they claim that", "they want to", "opponents believe", "the other side says"]

checkCircularReasoning :: [Proposition] -> [Fallacy]
checkCircularReasoning props
  | length props < 2 = []
  | otherwise =
      [ Fallacy 0 "Circular Reasoning" desc Critical (propText p)
      | p <- premises
      , c <- conclusions
      , isSimilar (propText p) (propText c)
      , let desc = "Premise repeats the conclusion: \"" <> propText p <> "\" mirrors \"" <> propText c <> "\""
      ]
  where
    premises    = filter (\p -> propType p == Premise) props
    conclusions = filter (\p -> propType p == Conclusion) props

checkFalseDisjunction :: [Proposition] -> [Fallacy]
checkFalseDisjunction props =
  [ Fallacy 0 "False Dilemma" desc Medium (propText p)
  | p <- props
  , let txt = T.toLower (propText p)
  , any (`T.isInfixOf` txt) falseDisjPatterns
  , let desc = "Presenting only two options when more exist: \"" <> propText p <> "\""
  ]
  where
    falseDisjPatterns = ["either you", "only two choices", "you must choose", "there are only", "only option"]

checkAppealToAuthority :: [Proposition] -> [Fallacy]
checkAppealToAuthority props =
  [ Fallacy 0 "Appeal to Authority" desc Low (propText p)
  | p <- props
  , let txt = T.toLower (propText p)
  , any (`T.isInfixOf` txt) authorityPatterns
  , let desc = "Relying on authority rather than evidence: \"" <> propText p <> "\""
  ]
  where
    authorityPatterns = ["experts say", "scientists agree", "according to authorities", "everyone knows"]

checkSlipperySlope :: [Proposition] -> [Fallacy]
checkSlipperySlope props =
  [ Fallacy 0 "Slippery Slope" desc Medium (propText p)
  | p <- props
  , let txt = T.toLower (propText p)
  , any (`T.isInfixOf` txt) slopePatterns
  , let desc = "Assuming one event will lead to extreme consequences: \"" <> propText p <> "\""
  ]
  where
    slopePatterns = ["will lead to", "next thing you know", "inevitably", "this will cause", "eventually"]

checkRedHerring :: [Proposition] -> [Fallacy]
checkRedHerring props =
  [ Fallacy 0 "Red Herring" desc Low (propText p)
  | p <- props
  , let txt = T.toLower (propText p)
  , any (`T.isInfixOf` txt) herringPatterns
  , let desc = "Distracting from the main argument: \"" <> propText p <> "\""
  ]
  where
    herringPatterns = ["but what about", "the real issue", "speaking of which", "let's talk about something"]

-- | Evaluate argument strength based on number of supporting premises and structure
evaluateArgument :: [Proposition] -> ArgumentScore
evaluateArgument props
  | null props = ArgumentScore 0.0 "No propositions to evaluate" [] ["No argument provided"]
  | otherwise = ArgumentScore score reasoning strengths weaknesses
  where
    premises    = filter (\p -> propType p == Premise) props
    conclusions = filter (\p -> propType p == Conclusion) props
    hypotheses  = filter (\p -> propType p == Hypothesis) props
    totalProps  = length props
    numPremises = length premises

    -- Score components
    premiseScore
      | numPremises >= 3 = 0.4
      | numPremises == 2 = 0.3
      | numPremises == 1 = 0.15
      | otherwise        = 0.0

    conclusionScore
      | length conclusions == 1 = 0.2
      | length conclusions > 1  = 0.1
      | otherwise               = 0.0

    diversityScore
      | totalProps >= 4 = 0.2
      | totalProps >= 2 = 0.1
      | otherwise       = 0.05

    keywordScore
      | null conclusions || null premises = 0.0
      | otherwise =
          let premWords = concatMap (getKeywords . propText) premises
              concWords = concatMap (getKeywords . propText) conclusions
              matches   = length $ filter (`elem` premWords) concWords
              ratio     = if null concWords then 0.0
                          else fromIntegral matches / fromIntegral (length concWords)
          in ratio * 0.2

    score = min 1.0 (premiseScore + conclusionScore + diversityScore + keywordScore)

    reasoning = "Argument strength evaluated based on " <> tshow numPremises
                <> " premise(s), " <> tshow (length conclusions) <> " conclusion(s), and "
                <> tshow (length hypotheses) <> " hypothesis/hypotheses. Overall score: "
                <> tshow (round' score) <> "%"

    strengths = concat
      [ ["Multiple supporting premises" | numPremises >= 2]
      , ["Clear conclusion stated" | length conclusions == 1]
      , ["Well-structured argument with diverse proposition types" | length (filter (\p -> propType p /= Premise) props) > 0 && numPremises > 0]
      ]

    weaknesses = concat
      [ ["No premises provided" | numPremises == 0]
      , ["Only one premise - needs more support" | numPremises == 1]
      , ["No conclusion stated" | null conclusions]
      , ["Multiple conclusions may weaken focus" | length conclusions > 1]
      , ["Insufficient proposition diversity" | totalProps < 2]
      ]

-- | Summarize an argument chain
summarizeArgument :: [Proposition] -> ArgumentSummary
summarizeArgument props
  | null props = ArgumentSummary "No propositions to summarize" 0 "N/A" []
  | otherwise = ArgumentSummary summary premCount concText flow
  where
    premises    = filter (\p -> propType p == Premise) props
    conclusions = filter (\p -> propType p == Conclusion) props
    premCount   = length premises

    concText = case conclusions of
      (c:_) -> propText c
      []    -> "No explicit conclusion stated"

    summary = "This argument consists of " <> tshow (length props)
              <> " proposition(s) with " <> tshow premCount
              <> " premise(s) leading to "
              <> (if null conclusions then "no explicit conclusion"
                  else tshow (length conclusions) <> " conclusion(s)")
              <> "."

    flow = map (\p -> propTypeLabel (propType p) <> ": " <> propText p) props

    propTypeLabel Premise    = "PREMISE"
    propTypeLabel Conclusion = "CONCLUSION"
    propTypeLabel Hypothesis = "HYPOTHESIS"

-- Helper functions

-- | Extract meaningful keywords from text (words > 3 chars, lowercased)
getKeywords :: Text -> [Text]
getKeywords = filter (\w -> T.length w > 3 && w `notElem` stopWords)
            . map T.toLower
            . T.words
            . T.filter (\c -> c == ' ' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))

stopWords :: [Text]
stopWords = ["that", "this", "with", "from", "have", "will", "been", "were",
             "they", "their", "them", "than", "then", "when", "what", "which",
             "there", "these", "those", "would", "could", "should", "about",
             "into", "more", "some", "such", "only", "also", "very", "just",
             "because", "does", "each", "before", "after"]

-- | Check if two texts are similar (share >50% keywords)
isSimilar :: Text -> Text -> Bool
isSimilar t1 t2 =
  let w1 = getKeywords t1
      w2 = getKeywords t2
      common = length $ filter (`elem` w2) w1
      total = max (length w1) (length w2)
  in total > 0 && (fromIntegral common / fromIntegral total :: Double) > 0.5

-- | Round a double to percentage
round' :: Double -> Int
round' d = round (d * 100)

tshow :: Show a => a -> Text
tshow = T.pack . show
