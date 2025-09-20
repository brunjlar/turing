# AGENTS.md

## Mini Index
- **Preflight**: `which git || true; git --version || true; which docker || true; docker --version || true; ssh-agent sanity check; ssh -T git@github.com || true`
- **Verification Template**: Problem · Acceptance criteria · Steps · Evidence · Rollback (per global instructions §0).
- **Live Notes**: Timestamped entries using Plan/Verify/Done/Next blocks (template in §13 global doc).

## Live Action Notes
2025-09-20 20:20 UTC — Append-bar example
Plan:
- Problem: Provide a reusable example that appends a trailing '|' to unary inputs while ensuring the rules are covered by automated tests.
- Acceptance criteria:
  * `examples/append-bar.rules` rewrites any string of '1's (including empty) to append a trailing '|'.
  * Automated tests (QuickCheck plus concrete cases) guard the behaviour and pass under `-Werror`.
  * Example terminates without relying on `--max-steps` safety limits.
- Steps:
  * Adopt the sentinel-based rules from `foo.rules`, explain the guard rule, and install them as `examples/append-bar.rules`.
  * Extend the test suite with deterministic checks and a QuickCheck property over bounded unary inputs.
  * Run `cabal build` + `cabal test` and capture the relevant evidence.
Verify:
- Commands: `cabal build`, `cabal test`.
- Evidence: both exit 0; QuickCheck reports `appends exactly one bar: OK`.
- Rollback: `git checkout -- examples/append-bar.rules test/Main.hs turing.cabal AGENTS.md` if needed.

- Note: Prefer prototyping rewrites with existing Haskell helpers to avoid semantic drift.
Done:
- Moved the working rule set into `examples/append-bar.rules` with inline notes about the no-op guard and cursor mechanics.
- Added hspec/QuickCheck coverage that reads the example, asserts concrete traces (empty, short, long), and proves `trace rules input` ends at `1^n|` for random unary inputs.
- Reintroduced the QuickCheck dependency in `turing.cabal`; `cabal build` and `cabal test` both pass.
Next:
- None; task complete.
2025-09-20 20:10 UTC — Add non-interactive CLI mode
Plan:
- Problem: The executable currently only supports interactive tracing via the REPL, so it cannot be scripted to trace a specific input string and exit after printing the result.
- Acceptance criteria:
  * CLI accepts an optional `--input` argument and exits after printing the trace when provided.
  * CLI supports an optional `--max-steps` argument that limits the number of rewrite steps emitted (inclusive of the starting state).
  * New and existing unit tests cover option parsing and limited trace output behaviour and all tests pass under `-Werror`.
  * Interactive REPL behaviour remains unchanged when `--input` is omitted.
- Steps:
  * Add failing tests for option parsing (including `--input`/`--max-steps`) and trace limiting helper logic.
  * Extend CLI options data type/parser and implement a reusable trace-printing helper with optional step cap.
  * Update `Main` to dispatch between non-interactive run and REPL, ensuring shared output formatting.
  * Run verification commands and capture evidence, updating docs if behaviour changes.
Verify:
- Commands: `cabal build`, `cabal test`, `cabal run turing -- test/data/sample.rules --input abc`, `cabal run turing -- test/data/sample.rules --input abc --max-steps 1`.
- Evidence: exit codes (0), tail of non-interactive outputs showing numbered trace with and without step limit.
- Rollback: `git checkout -- app/Main.hs src/Rewrite/Repl.hs src/Turing/CLI.hs test/Main.hs turing.cabal`.
Done:
- Added tests guarding `--input`/`--max-steps` parsing and trace limiting, implemented non-interactive run path with reusable helpers, and documented the new flags.
- `cabal test`, `cabal build`, `cabal run turing -- test/data/sample.rules --input abc`, and `cabal run turing -- test/data/sample.rules --input abc --max-steps 1` each exited 0 with expected trace output.
Next:
- None; ready for handoff.
2025-09-20 17:12 UTC — Document CLI additions
Plan:
- Problem: Project changelog lacks notes about the new optparse CLI and interactive trace REPL behaviour, so documentation is out of date.
- Acceptance criteria:
  * `CHANGELOG.md` gains an "Unreleased" section mentioning the CLI help improvements and interactive REPL trace support.
- Steps:
  * Edit `CHANGELOG.md` to add the new section and bullet points.
Verify:
- Manual: confirm the changelog renders the new section and text; no commands required.
- Rollback: `git checkout -- CHANGELOG.md` if wording needs reset.
Done:
- `CHANGELOG.md` now includes an "Unreleased" section documenting the optparse-based CLI help text and interactive REPL trace behaviour.
Next:
- None; documentation updated.
2025-09-20 17:08 UTC — Verify CLI REPL behaviour
Plan:
- Problem: Confirm the optparse-applicative CLI wiring and REPL loop behave as expected by building, running tests, and exercising the executable with sample input.
- Acceptance criteria:
  * `cabal build` and `cabal test` succeed without warnings under `-Werror`.
  * `cabal run turing -- --help` shows the new help/usage text.
  * Supplying `abc` via stdin to `cabal run turing -- test/data/sample.rules` prints numbered trace steps for each rewrite.
- Steps:
  * Run the build and unit tests to ensure code compiles and tests stay green.
  * Capture `--help` output for documentation.
  * Pipe a sample string through the executable and confirm the trace output format.
Verify:
- Commands: `cabal build`, `cabal test`, `cabal run turing -- --help`, `printf 'abc\\n' | cabal run turing -- test/data/sample.rules`.
- Evidence: exit codes (expect 0), tail of `--help` and pipeline output showing numbered trace, noting interactive prompt handling.
- Rollback: No code changes expected; if required, `git checkout -- AGENTS.md` to revert notes.
Done:
- Build, test, and manual CLI checks all succeeded: `cabal build`, `cabal test`, `cabal run turing -- --help`, and piping `abc` through `cabal run turing -- test/data/sample.rules` (each exit 0 with expected help + trace output).
Next:
- None; verification complete.
2025-09-20 15:15 UTC — Introduce optparse CLI and interactive trace REPL
Plan:
- Problem: CLI needs optparse-applicative help/usage output while preserving rules-file argument and extending behaviour with an interactive trace REPL that respects Ctrl-C.
- Acceptance criteria:
  * New optparse-applicative parser yields helpful `--help` output and accepts a mandatory rules file path.
  * After printing parsed rules, the executable enters a loop reading lines, printing each rewrite trace, and tolerates Ctrl-C without leaving terminal in inconsistent state.
  * Existing parser tests stay green and new tests cover CLI option parsing and trace formatting helper logic.
  * `cabal build` and `cabal test` complete without warnings (still under `-Werror`).
- Steps:
  * Add failing tests for CLI parsing and trace rendering utility to drive REPL output structure.
  * Implement optparse-applicative based CLI, REPL loop, and graceful Ctrl-C handling; update Cabal dependencies.
  * Run verification commands and capture evidence; document any manual verification gaps.
Verify:
- Commands: `cabal build`, `cabal test`, `cabal run turing -- --help`, `printf 'abc\\n' | cabal run turing -- test/data/sample.rules`.
- Evidence: exit codes, tail of CLI output (≤200 lines) confirming help text and trace interaction.
- Rollback: `git checkout -- app/Main.hs src/Rewrite.hs test/Main.hs turing.cabal`.
Done:
- Added `Turing.CLI` optparse-applicative parser with help metadata and tests for required rules argument success/failure.
- Introduced `Rewrite.Repl` with `renderTraceLines` (numbered steps) and `runRepl` handling Ctrl-C during traces while exiting on input interrupt; unit test covers formatting.
- Updated `app/Main.hs` to use new parser + REPL, preserving rules printing; cabal file exposes new modules and dependencies.
- Evidence gathered: `cabal test`, `cabal build`, `cabal run turing -- --help`, `printf 'abc\n' | cabal run turing -- test/data/sample.rules` (all exit 0).
Next:
- None; task complete.
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
- 2025-09-20: Apparent impossibility came from assuming the dot cursor needed a fresh sentinel; existing rules already provide a neutral guard. Lesson: test for termination by simulating `Rewrite.trace` rather than reasoning purely about rule forms.
- 2025-09-20: Empty-LHS rules can terminate cleanly when paired with a no-op guard (e.g., `| -> |;`) that short-circuits `step`; pointer rules (`1 -> .1`, `.1 -> 1.`) move a cursor without extra sentinels. Challenge every assumption and verify it via `Rewrite.trace` before declaring limits.
- 2025-09-19: Repo contains `LICENSE` and `run-codex.sh`; AGENTS.md added for local protocols.
- 2025-09-19: `cabal.project` enables `tests: True` for reproducible `cabal test`. Requires network to fetch tasty/hspec/QuickCheck from Hackage.
