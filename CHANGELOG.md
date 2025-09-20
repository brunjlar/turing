# Revision history for turing

## Unreleased

* Switch the CLI to `optparse-applicative`, providing a helpful `--help` banner and mandatory rules file argument validation.
* Add an interactive trace REPL that prints numbered rewrite steps, tolerates Ctrl-C interruptions, and re-prompts cleanly.
* Introduce a non-interactive mode (`--input` with optional `--max-steps`) that prints a single trace and exits, enabling scripted runs.

## 0.1.0.0 -- 2025-09-19

* Set up the Cabal project with a library, executable, and tasty-based test suite.
