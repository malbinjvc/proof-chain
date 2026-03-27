{-# LANGUAGE OverloadedStrings #-}

module ProofChain.Services
  ( createProposition
  , getProposition
  , listPropositions
  , deleteProposition
  , getStats
  , lookupPropositions
  , incrementProofsValidated
  , incrementFallaciesFound
  , incrementArgsEvaluated
  ) where

import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)

import ProofChain.Types

-- | Create a new proposition and store it
createProposition :: AppState -> Text -> PropositionType -> IO Proposition
createProposition state txt pt = do
  now <- getCurrentTime
  pid <- readIORef (stateNextPropId state)
  let prop = Proposition
        { propId = pid
        , propText = txt
        , propType = pt
        , propCreatedAt = now
        }
  modifyIORef' (statePropositions state) (Map.insert pid prop)
  modifyIORef' (stateNextPropId state) (+ 1)
  return prop

-- | Get a specific proposition by ID
getProposition :: AppState -> Int -> IO (Maybe Proposition)
getProposition state pid = do
  props <- readIORef (statePropositions state)
  return $ Map.lookup pid props

-- | List all propositions
listPropositions :: AppState -> IO [Proposition]
listPropositions state = do
  props <- readIORef (statePropositions state)
  return $ Map.elems props

-- | Delete a proposition by ID, returns True if it existed
deleteProposition :: AppState -> Int -> IO Bool
deleteProposition state pid = do
  props <- readIORef (statePropositions state)
  if Map.member pid props
    then do
      modifyIORef' (statePropositions state) (Map.delete pid)
      return True
    else return False

-- | Look up multiple propositions by IDs
lookupPropositions :: AppState -> [Int] -> IO (Either Text [Proposition])
lookupPropositions state ids = do
  props <- readIORef (statePropositions state)
  let results = map (\pid -> Map.lookup pid props) ids
      missing = [ pid | (pid, Nothing) <- zip ids results ]
  if null missing
    then return $ Right [ p | Just p <- results ]
    else return $ Left $ "Propositions not found: " <> showIds missing

showIds :: [Int] -> Text
showIds ids = mconcat $ zipWith (\i idx -> (if idx > (0 :: Int) then ", " else "") <> tshow i) ids [0..]

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Get current statistics
getStats :: AppState -> IO Stats
getStats state = Stats
  <$> (Map.size <$> readIORef (statePropositions state))
  <*> readIORef (stateProofsValidated state)
  <*> readIORef (stateFallaciesFound state)
  <*> readIORef (stateArgsEvaluated state)

-- | Increment proof validation counter
incrementProofsValidated :: AppState -> IO ()
incrementProofsValidated state = modifyIORef' (stateProofsValidated state) (+ 1)

-- | Increment fallacies found counter
incrementFallaciesFound :: AppState -> Int -> IO ()
incrementFallaciesFound state n = modifyIORef' (stateFallaciesFound state) (+ n)

-- | Increment arguments evaluated counter
incrementArgsEvaluated :: AppState -> IO ()
incrementArgsEvaluated state = modifyIORef' (stateArgsEvaluated state) (+ 1)
