# Learning the Rules Language

Rules is a deterministic string-rewriting language.  A program is a list of clauses that are scanned from top to bottom until one of them changes the current string.  The new string becomes the next state and the process repeats until no rule applies.  This tutorial introduces the syntax, explains the execution model, and walks through strategies for solving increasingly rich problems.

> **Tip:** All examples in this document are exercised by the automated test suite (`cabal test`).  If you copy the snippets into your own rules files, the behaviour you see on the command line will match what is documented here.

## 1. Syntax at a glance

A Rules file is made of clauses with the shape:

```text
lhs -> rhs;
```

- **`lhs`** (left-hand side) is the pattern that must appear in the current string.
- **`rhs`** (right-hand side) is the replacement text.
- Every rule ends with a semicolon.

Whitespace between tokens is ignored, and you can sprinkle both line (`-- like this`) and block (`{- like this -}`) comments anywhere.  Literal whitespace belongs inside a rule either directly or through escape sequences:

| Escape | Meaning |
| ------ | ------- |
| `\n`   | newline |
| `\t`   | tab |
| `\r`   | carriage return |
| `\s`   | a single space |
| `\\`  | a literal backslash |

If the `lhs` is empty (`-> ...;`), the rule always matches the current prefix.  You can use this to seed sentinels or handle edge cases.

## 2. How evaluation works

Evaluation is leftmost-first and rule-ordered:

1. Start at the first rule.
2. Search for the rule’s `lhs` starting at the left edge of the string.  If no prefix matches, slide one character to the right and try again.
3. As soon as a match is found, replace it with the `rhs`.  That becomes the next string in the trace.
4. If the rule makes no change (the `rhs` is identical to what it replaced), the engine ignores it to avoid infinite loops.
5. If no rule changes the string, the program halts.

The helper `trace` function (and the CLI) show the entire history:

```text
step 0: initial input
step 1: after the first rewrite
...
```

The guard rule `| -> |;` appears frequently in examples.  Because the engine stops after any change-less rule, this “do nothing” clause freezes the system once a `|` symbol enters the string.

## 3. First example: appending a guard

[`examples/append-bar.rules`](examples/append-bar.rules) appends a trailing `|` to unary strings (possibly the empty string):

```text
step 0: 111
step 1: .111
step 2: 1.11
step 3: 11.1
step 4: 111.
step 5: 111|
```

How it works:

1. `1 -> .1;` introduces a “cursor” (`.`) immediately before the first `1`.
2. `.1 -> 1.;` bubbles that cursor to the right, one digit at a time.
3. `. -> |;` replaces the terminal cursor with the guard bar.
4. `-> |;` handles the empty input by emitting a bar straight away.
5. `| -> |;` ensures that once a bar appears, no further rules fire.

This demonstrates a recurring technique: use a cursor to walk the string and let a no-op guard absorb further work.

## 4. Building pipelines: duplicating a unary string

[`examples/duplicate.rules`](examples/duplicate.rules) turns `111` into `111|111`.  The program first appends its own sentinel (`#`), then stages several passes:

1. **Stage 0:** A cursor walks to the end and drops `#` so callers never have to provide it.
2. **Stage 1:** As the sentinel slides left (`1# -> #1b;`), each consumed `1` becomes a marker `b` for the copy under construction.
3. **Stage 2:** Rules like `b1 -> 1b;` and `@1 -> 1@;` reorder the markers so that originals sit left of the separator while the copy grows on the right.
4. **Stage 3:** Once the separator reaches the front, `b -> 1;` and `@ -> |;` expose the final `left|right` view.
5. **Guard:** `| -> |;` stops everything once duplication is complete.

Because the rules add their own sentinel and guard, you can run the program on the bare payload.  The test suite covers the empty string, single digits, and longer inputs to ensure the doc stays accurate.

## 5. Stateful algorithms: unary multiplication

[`examples/unary-multiply.rules`](examples/unary-multiply.rules) multiplies two unary numbers separated by `*`.  For example, `11*111` rewrites to `111111`.

Key ideas:

- **Sentinel bootstrapping:** `* -> ^#;` replaces the separator with a control marker (`^`) and drops a trailing `|` so later stages know where “output” lives.
- **Active passes:** `1^ -> @x;` marks the left operand that is currently being multiplied, while `x1 -> rx;` sweeps the right operand and tags each `1` as `r`.
- **Pointer shuttling:** Once the sweep hits the bar, rules like `c| -> d|;` and `d| -> |e1;` append a copied `1` to the output while transporting a pointer (`e`) back to the staging area.
- **Resetting state:** `@b -> ^;` restores the idle separator so the next left `1` can trigger another pass.  When the left operand is exhausted, Stage 4 collapses the scaffolding and leaves only the product.

The tests exercise zero operands, concrete examples, and a QuickCheck property over small lengths.

## 6. Working with binary data: incrementing

[`examples/binary-increment.rules`](examples/binary-increment.rules) increments a binary string and appends a guard bar.  Removing the bar yields the numeric successor, e.g. `11111` becomes `100000`.

- Stage 0 appends a carry marker `+` by walking a cursor to the end.
- Stage 1 resolves the carry with rules such as `1+ -> +0;` and `0+ -> 1|;`.  The bar bubbles to the right via `|0 -> 0|;` and `|1 -> 1|;` so it always trails the result.
- The guard `| -> |;` freezes the program when the carry work is finished.

The property test picks random inputs up to 512 and checks that stripping the guard reproduces `n + 1` in binary.

## 7. Strategies and tricks

- **Work left-to-right.** Because matching is leftmost-first, structure your rules so that early clauses initialise state, and later ones tidy up.  Guard clauses (`| -> |;`) protect the finished result from being reprocessed.
- **Introduce markers.** Temporary symbols (`.`, `@`, `b`, `+`, etc.) turn the string into a miniature state machine.  Plan your pipeline as phases and dedicate a few unique markers to each phase.
- **Add your own sentinels.** Don’t force callers to append `#` or `|`.  Instead, spend a few setup rules to add the sentinel automatically.  It keeps programs composable and easier to test.
- **Reset after every pass.** Complex programs (like multiplication) repeatedly consume part of the input and then restore the initial markers.  This ensures that subsequent passes see the same structure.
- **Prove behaviour with traces.** Use `cabal run turing -- FILE --input STRING` during development.  The numbered trace quickly reveals where a pipeline stalls or loops.

## 8. Testing your own rules

Add new examples under `examples/` and extend `test/Main.hs` with deterministic checks (exact traces or final states) plus property tests when possible.  Running `cabal test` recompiles the rules and fails fast if a documentation example goes stale.

Happy rewriting!
