# AGENTS.md

## Mini Index
- **Preflight**: `which git || true; git --version || true; which docker || true; docker --version || true; ssh-agent sanity check; ssh -T git@github.com || true`
- **Verification Template**: Problem · Acceptance criteria · Steps · Evidence · Rollback (per global instructions §0).
- **Live Notes**: Timestamped entries using Plan/Verify/Done/Next blocks (template in §13 global doc).

## Live Action Notes
2025-09-20 14:50 UTC — Remove colon requirement and enforce -Werror
Plan:
- Problem: Parser syntax must drop leading ':' while still handling whitespace/escapes, and build should treat warnings as errors.
- Acceptance criteria:
  * All parser specs updated for new syntax (including literal backslash coverage) pass.
  * CLI processes colon-less rules correctly.
  * `cabal build`/`cabal test` succeed with `-Werror` enabled (no warnings).
- Steps:
  * Update tests/fixtures to capture new grammar and escaping behaviour.
  * Modify `Rewrite.Parser` (and helpers) to accept colon-less rules.
  * Adjust cabal common warnings to include `-Werror`.
  * Rebuild, rerun tests, and confirm CLI behaviour.
Verify:
- Commands: `cabal build`, `cabal test`, `cabal run turing -- test/data/sample.rules`.
- Evidence: exit codes/log snippets ≤200 lines.
- Rollback: `git checkout -- app/Main.hs src/Rewrite.hs src/Rewrite/Parser.hs test/Main.hs test/data/sample.rules turing.cabal` if necessary.
Done:
- Parser specs cover colon-less syntax, whitespace duplication via escapes, literal backslashes, and failure messaging.
- `Rewrite.Parser` updated to accept rules without leading colon while preserving intentional leading spaces and skipping inter-rule whitespace/comments.
- Cabal `common warnings` now enforces `-Wall -Werror`; CLI parses/prints updated sample rules fixture.
- Evidence captured:
  * `cabal build` (exit 0)
  * `cabal test` (parser suite 7/7 green)
  * `cabal run turing -- test/data/sample.rules` → `[Rule " " "  ",Rule "abc" "ABC"]`
Next:
- None; task complete.
2025-09-20 14:36 UTC — Implement rules parser and CLI integration
Plan:
- Problem: Need a robust parser for textual rules with whitespace/comments and to hook it into CLI plus tests.
- Acceptance criteria:
  * `parseRules` parses representative success cases and returns `Rules Char` matching expectations.
  * Parsing failures yield informative `Left` messages (non-empty, mention location).
  * CLI accepts a rules file argument, reports parse errors with non-zero exit, prints parsed rules otherwise.
- Steps:
  * Review existing modules/dependencies for parser integration.
  * Add failing tests covering parser success/failure and CLI behaviour surface.
  * Implement parser module and supporting library changes, update cabal dependencies.
  * Wire parser into executable output flow.
Verify:
- Commands to run:
  * `cabal build`
  * `cabal test`
  * `cabal run turing -- test/data/sample.rules`
- Evidence to capture: exit codes and concise command output summaries (<200 lines).
- Rollback/Cleanup: `git checkout -- .` and remove temporary fixtures under `test/data/` if introduced.
Done:
- Added targeted hspec specs for parser happy-paths, whitespace handling, escapes, error reporting.
- Implemented `Rewrite.Parser` using Megaparsec with comment-aware whitespace trimming, escape handling, and preserved whitespace-only rules; derived `Show` for `Rule`.
- Updated CLI to read a rules file, parse with error propagation to stderr, and print parsed rules; added sample fixture.
- Commands executed with success:
  * `cabal build`
  * `cabal test`
  * `cabal run turing -- test/data/sample.rules`
Next:
- None; task complete.

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
