# AGENTS.md

## Mini Index
- **Preflight**: `which git || true; git --version || true; which docker || true; docker --version || true; ssh-agent sanity check; ssh -T git@github.com || true`
- **Build/Test loop**: Always run `cabal update` before `cabal test`; the container has full network access so dependency downloads succeed.
- **Verification Template**: Problem · Acceptance criteria · Steps · Evidence · Rollback (per global instructions §0).
- **Tutorial upkeep**: Whenever new examples or Rules strategies land, update `tutorial.md` and its tests before considering the work done.

## Project Knowledge Base
### Build & Verification
- Run `cabal update` once per session (or before `cabal test`) to avoid missing-dependency failures.
- Standard verification bundle: `cabal build`, `cabal test`, `cabal run turing -- --help`, and targeted example runs as needed. Capture exit codes and trim logs ≤200 lines.
- If tests unexpectedly fail because packages are missing, rerun `cabal update --index-state <timestamp>` and retry.

### CLI Usage
- Prefer the compiled executable: `turing RULES --input STRING` prints a finite trace and exits. `cabal run turing -- RULES` still works for ad-hoc traces/REPLs.
- `cabal run turing -- <rules-file>` prints parsed rules, then enters a REPL that tolerates Ctrl-C. Pipe sample input to trace rewrites, e.g. `printf 'abc\n' | cabal run turing -- test/data/sample.rules`.

### Rules Programming Tips
- Unary multiplication: reuse the `* -> ^#` sentinel trick, operate in the `@` active state for controlled passes, and restore `^` at the end.
- Unary duplication: append a trailing `#`, shuttle with `b/c/d/e` markers, then reveal via `@ -> |; b -> 1` to terminate cleanly.
- Binary addition: the repeated-increment example stages input as `sLEFT#@RIGHT$`; drop the leading `s` after termination to read the sum.
- Cursor movement: Empty-LHS rules can terminate safely when paired with a guard like `| -> |;`; pointer rules (`1 -> .1`, `.1 -> 1.`) move cursors without extra sentinels.
- Validate termination assumptions with `Rewrite.trace` instead of pure reasoning.

## Live Action Notes (Scratchpad)
Use this section only for the **current** task. Follow the template below, delete the entry once the task is complete, and promote any durable lessons to the sections above.

```
YYYY-MM-DD HH:MM — Task name
Plan:
- Step 1 …
- Step 2 …
Verification Plan:
- Problem — …
- Acceptance criteria — …
- Steps — …
- Evidence — …
- Rollback/Cleanup — …
Verify:
- Commands/evidence …
Done:
- Summary once finished.
Next:
- Single next action or “None”.
```


## Maintenance Protocol
- Before starting work, skim the Mini Index and existing Live Action Notes.
- While working, keep only one active Live Action entry; remove or archive it immediately after completion.
- Promote repeatable commands, quirks, or strategies to the Project Knowledge Base with timestamps if helpful; do **not** leave completed activity logs.
- If an entry no longer provides future value, delete it instead of leaving stale history.
