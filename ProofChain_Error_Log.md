# ProofChain Error Log

## Project: ProofChain
- **Language**: Haskell (GHC 9.14.1 local, GHC 9.8 CI)
- **Framework**: Warp + WAI (originally Scotty)
- **Date**: 2026-03-27

---

## Error 1: Scotty 0.30 Incompatible with GHC 9.14.1

**Error**:
```
Web/Scotty/Action.hs:459:66: error: [GHC-83865]
  Couldn't match expected type: Map.Map T.Text [T.Text]
                with actual type: HM.HashMap T.Text [T.Text]
```

**Cause**: Scotty 0.30's `paramListToForm` function uses `HM.HashMap` (from `unordered-containers`) but GHC 9.14.1's version of `http-types` changed the `Form` type to use `Map.Map` (from `containers`). This is a type incompatibility between Scotty 0.30 and the newer GHC 9.14.1 ecosystem.

**Fix**: Replaced Scotty with Warp + WAI direct routing. Rewrote `App.hs` to use WAI `Application` type with manual pattern matching on `requestMethod` and `pathInfo`. All routes preserved with identical behavior. `hspec-wai` tests required no changes since they work with any WAI Application.

---

## Error 2: Type Error in Clients.hs - Proposition vs PropositionType

**Error**:
```
src/ProofChain/Clients.hs:193:98: error: [GHC-83865]
  Couldn't match type 'Proposition' with 'PropositionType'
```

**Cause**: `filter (/= Premise) props` compared a `Proposition` record with a `PropositionType` constructor (`Premise`). These are different types - `Proposition` is a record containing a `propType :: PropositionType` field.

**Fix**: Changed to `filter (\p -> propType p /= Premise) props` to extract the type field before comparison.

---

## Error 3: cabal.project allow-newer

**Error**: Dependency resolution failures with GHC 9.14.1 and newer package versions.

**Cause**: GHC 9.14.1 ships with newer base library versions that some packages haven't updated their bounds for.

**Fix**: Added `allow-newer: all` to `cabal.project` to relax version constraints.

---

## Error 4: Docker Verify Step Hanging

**Error**: CI Docker job hung indefinitely on "Verify Docker image" step.

**Cause**: `docker run --rm proof-chain:test --help` passed `--help` to the Warp server binary, which doesn't handle CLI arguments — it starts listening on port 8080 and never exits. The `|| true` suffix only applies after process exit.

**Fix**: Changed verify step to start container in detached mode (`-d`), wait 3 seconds, check logs, then stop. Also fixed HEALTHCHECK from `CMD ["./proof-chain", "--help"]` to `CMD curl -f http://localhost:8080/health` and added `curl` to runtime image.

---

## Error 5: Git Remote Cross-Wiring

**Error**: ProofChain's GitHub repo had SignalMesh code instead of ProofChain code.

**Cause**: During initial `gh repo create`, the signal-mesh local repo's remote was set to `proof-chain.git` and pushed SignalMesh's initial commit there. ProofChain local repo had no remote configured.

**Fix**: Added correct remote to ProofChain local repo, force-pushed ProofChain code to overwrite the wrong content. Fixed SignalMesh's remote URL to point to `signal-mesh.git`.

---

## Summary
- Total errors encountered: 5
- All resolved successfully
- Tests passing: 39/39
- CI status: All green (GHC 9.8)
