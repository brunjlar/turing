# AGENTS.md

## Mini Index
- **Preflight**: `which git || true; git --version || true; which docker || true; docker --version || true; ssh-agent sanity check; ssh -T git@github.com || true`
- **Verification Template**: Problem · Acceptance criteria · Steps · Evidence · Rollback (per global instructions §0).
- **Live Notes**: Timestamped entries using Plan/Verify/Done/Next blocks (template in §13 global doc).

## Live Action Notes
2025-09-19 20:10 UTC — Initialize Cabal project with tests
Plan:
- Review repository for nested instructions (AGENTS.md) before generating files.
- Run `cabal init` to scaffold library, executable, and test-suite using tasty with hspec/QuickCheck.
- Implement sample library functions, executable, and tests following TDD loop.
- Run formatting or linting if required, build project, and execute tests.
- Update documentation/scratchpad as needed and prepare commit with evidence.
Verify:
- Commands: `ls`, `rg --files -g 'AGENTS.md'`, `cabal build`, `cabal test`.
- Capture test output (<200 lines) and ensure exit codes are 0.
- Record git status before commit; ensure clean tree after commit.
Done:
- Scaffolded Cabal project with library/executable/test-suite, implemented sample functions, and documented changes.
- `cabal build` succeeds; `cabal test` blocked by missing tasty packages because Hackage index cannot be downloaded in this environment.
Next:
- If network access becomes available, run `cabal update` then `cabal test` to install tasty/hspec/QuickCheck packages.

## Scratchpad
- 2025-09-19: Repo contains `LICENSE` and `run-codex.sh`; AGENTS.md added for local protocols.
- 2025-09-19: `cabal.project` enables `tests: True` for reproducible `cabal test`. Requires network to fetch tasty/hspec/QuickCheck from Hackage.
