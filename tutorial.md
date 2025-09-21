# Learning the Rules Language

Rules is a deterministic string-rewriting language. A program is a list of clauses that are scanned from top to bottom until one of them changes the current string. The new string becomes the next state and the process repeats until no rule applies. This tutorial is meant to be your from-zero-to-hero guide: it starts with the core syntax, layers on progressively richer strategies, and ends with ideas for composing complete solutions.

> **Tip:** Every example in this document is exercised by the automated test suite (`cabal test`). When you copy the snippets into your own rules files, the behaviour you see on the command line will match what is documented here.

## 1. Syntax at a Glance

A Rules file is made of clauses with the shape:

```text
lhs -> rhs;
```

- **`lhs`** (left-hand side) is the pattern that must appear in the current string.
- **`rhs`** (right-hand side) is the replacement text.
- Every rule ends with a semicolon.

Whitespace between tokens is ignored and you can sprinkle both line (`-- like this`) and block (`{- like this -}`) comments anywhere. Literal whitespace belongs inside a rule either directly or through escape sequences:

| Escape | Meaning |
| ------ | ------- |
| `\n`   | newline |
| `\t`   | tab |
| `\r`   | carriage return |
| `\s`   | a single space |
| `\\`  | a literal backslash |

If the `lhs` is empty (`-> ...;`), the rule always matches the current prefix. You can use this to seed sentinels or handle edge cases.

## 2. Execution Model

Evaluation is leftmost-first and rule-ordered:

1. Start at the first rule.
2. Search for the rule’s `lhs` starting at the left edge of the string. If no prefix matches, slide one character to the right and try again.
3. As soon as a match is found, replace it with the `rhs`. That becomes the next string in the trace.
4. If the rule makes no change (the `rhs` is identical to what it replaced), the engine ignores it to avoid infinite loops.
5. If no rule changes the string, the program halts.

The helper `trace` function (and the CLI) show the entire history:

```text
step 0: initial input
step 1: after the first rewrite
...
```

The guard rule `| -> |;` appears frequently. Because the engine stops after any change-less rule, this "do nothing" clause freezes the system once a `|` symbol enters the string.

## 3. Quickstart: Appending a Guard

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

## 4. Building Pipelines: Duplicating a Unary String

[`examples/duplicate.rules`](examples/duplicate.rules) turns `111` into `111|111`. The program first appends its own sentinel (`#`), then stages several passes:

1. **Stage 0:** A cursor walks to the end and drops `#` so callers never have to provide it.
2. **Stage 1:** As the sentinel slides left (`1# -> #1b;`), each consumed `1` becomes a marker `b` for the copy under construction.
3. **Stage 2:** Rules like `b1 -> 1b;` and `@1 -> 1@;` reorder the markers so that originals sit left of the separator while the copy grows on the right.
4. **Stage 3:** Once the separator reaches the front, `b -> 1;` and `@ -> |;` expose the final `left|right` view.
5. **Guard:** `| -> |;` stops everything once duplication is complete.

Because the rules add their own sentinel and guard, you can run the program on the bare payload. The test suite covers the empty string, single digits, and longer inputs to ensure the documentation stays accurate.

## 5. Stateful Algorithms: Unary Multiplication

[`examples/unary-multiply.rules`](examples/unary-multiply.rules) multiplies two unary numbers separated by `*`. For example, `11*111` rewrites to `111111`.

Key ideas:

- **Sentinel bootstrapping:** `* -> ^#;` replaces the separator with a control marker (`^`) and drops a trailing `|` so later stages know where “output” lives.
- **Active passes:** `1^ -> @x;` marks the left operand currently being multiplied, while `x1 -> rx;` sweeps the right operand and tags each `1` as `r`.
- **Pointer shuttling:** Once the sweep hits the bar, rules like `c| -> d|;` and `d| -> |e1;` append a copied `1` to the output while transporting a pointer (`e`) back to the staging area.
- **Resetting state:** `@b -> ^;` restores the idle separator so the next left `1` can trigger another pass. When the left operand is exhausted, Stage 4 collapses the scaffolding and leaves only the product.

The tests exercise zero operands, concrete examples, and a QuickCheck property over small lengths.

## 6. Translating Number Systems: Unary → Binary

[`examples/unary-to-binary.rules`](examples/unary-to-binary.rules) converts a string of unary `1`s into its binary representation and leaves a trailing `#` sentinel. Removing the sentinel yields the canonical binary digits. Representative examples:

| Unary input | Final trace (`#` retained) | Binary value |
| ----------- | -------------------------- | ------------- |
| (empty)     | `0#`                        | `0`           |
| `111`       | `11#`                       | `11`          |
| `1111`      | `100#`                      | `100`         |
| `1111111111`| `1010#`                     | `1010`        |

Pipeline overview:

1. **Scaffold:** The tail rules at the bottom append `|0#` to every input by walking a cursor across the unary digits.
2. **Consume unary:** Each `1` to the left of `|` becomes a guard `g` and queues an increment (`1| -> g|a;`). The guard prevents the next unary digit from firing until the increment finishes.
3. **Seek the end:** `a0 -> 0a;` and `a1 -> 1a;` drift the active marker to the `#` terminator.
4. **Carry propagation:** At the terminus `a# -> b#;` swaps in a carry indicator. Moving back toward the front turns `0` into `1` and ripples carry over `1b -> b0;`.
5. **Cleanup:** Once the carry settles, the guard dissolves into plain digits (`g1 -> 1;`, `g# -> #;`) and the separator disappears (`|0 -> 0;`). The trailing `#` remains in place so downstream passes can detect completion.

The tests strip the `#` before comparing against `binaryString n`, guaranteeing the documentation and the implementation stay in lockstep.

To watch the trace yourself:

```bash
cabal run turing -- examples/unary-to-binary.rules --input 1111111111
```

The final line is `step …: 1010#`; removing `#` gives `1010` (decimal 10).

## 7. Translating Number Systems: Binary → Unary

[`examples/binary-to-unary.rules`](examples/binary-to-unary.rules) performs the inverse conversion for inputs up to six bits. Instead of simulating arithmetic, it uses a lookup table of rewrite clauses so each binary literal rewrites directly to the corresponding count of `1`s followed by `|`.

Highlights:

- Smaller literals appear later in the file so longer patterns match first (`100000` before `1`). This prevents partial matches on prefixes.
- Every rewritten output ends with `|`. The guard rule `| -> |;` sits at the top, halting the program immediately after a lookup succeeds.
- Tests remove the guard before asserting that the output length equals the decoded integer.

You can combine this with the previous section to build round-trips. For example, running `binary-to-unary.rules` after `unary-to-binary.rules` recovers the original unary payload (minus the guard) for all documented cases.

## 8. Carry Choreography: Incrementing Binary

[`examples/binary-increment.rules`](examples/binary-increment.rules) increments a binary string and appends a guard bar. Removing the bar yields the numeric successor (e.g., `11111` becomes `100000`).

- **Setup:** A cursor walks to the end and drops a carry marker `+`.
- **Resolution:** Rules such as `1+ -> +0;` and `0+ -> 1|;` manage the carry. The bar bubbles to the right via `|0 -> 0|;` and `|1 -> 1|;` so it always trails the result.
- **Guard:** `| -> |;` freezes the program when the carry work is finished.

The property test picks random inputs up to 512 and checks that stripping the guard reproduces `n + 1` in binary. Chaining this example after the unary→binary conversion and before the binary→unary lookup yields a complete "add one" pipeline on unary inputs.

## 9. Borrowing Back: Decrementing Binary

[`examples/binary-decrement.rules`](examples/binary-decrement.rules) handles the opposite operation: given a canonical binary
numeral followed by `-1`, it subtracts one, trims any leading zeros that appear, and reports underflow for `0-1`.

- **Borrow cursor:** The suffix `-1` becomes a cursor `p` that walks left while a sentinel `#` bubbles to the front.
- **Propagation:** Each `0` before the cursor flips to `1` as the borrow moves left. Encountering a `1` settles the borrow by
  turning it into `0` and leaving behind a cleanup marker `@` that the final stage erases.
- **Underflow guard:** If the cursor touches the sentinel, the program rewrites the entire string to the explicit error message
  `Error: Arithmetic underflow!`.

The documented samples evaluate exactly as advertised:

```text
1010-1  -> 1001
1001-1  -> 1000
1000-1  -> 111
11001-1 -> 11000
1-1     -> 0
0-1     -> Error: Arithmetic underflow!
```

The accompanying QuickCheck property picks numbers up to 512, feeds them through the rules, and checks that the output matches
`n - 1` (or the underflow message when `n = 0`).

## 10. Permutation Mechanics: Sorting Ternary Digits

[`examples/ternary-sort.rules`](examples/ternary-sort.rules) shows that Rules can do more than arithmetic. The program repeatedly swaps out-of-order neighbours so any string over `0`, `1`, and `2` settles into nondecreasing order.

```text
21 -> 12;
20 -> 02;
10 -> 01;
```

Because the engine is deterministic and always rewrites the leftmost match, these three clauses behave exactly like a bubble sort: every swap removes one inversion, and once none remain the program halts. Here is the full trace for `210201`:

```text
step 0: 210201
step 1: 120201
step 2: 102201
step 3: 102021
step 4: 102012
step 5: 100212
step 6: 100122
step 7: 010122
step 8: 001122
```

The test suite loads the same rules, asserts this trace verbatim, and adds a QuickCheck property that generates random ternary strings (up to length six) and confirms the final state equals `sort input`. This gives you a reusable pattern for any permutation logic: specify pairwise swaps and rely on monotone progress to guarantee termination.

## 11. Composition Playbook

Once you trust the building blocks above, try combining them:

- **Unary successor:** Apply `unary-to-binary.rules`, then `binary-increment.rules`, and finish with `binary-to-unary.rules`. The intermediate sentinels (`#` and `|`) make it easy to detect each stage’s completion.
- **Trace debugging:** Use `cabal run turing -- FILE --input STRING --max-steps N` to cap runaway traces while you experiment with new compositions.
- **Property checks everywhere:** Every time you document a behaviour, add a deterministic assertion and a QuickCheck property in `test/Main.hs`. That way the docs and programs evolve together.

## 12. Strategies and Tricks

- **Work left-to-right.** Because matching is leftmost-first, structure your rules so early clauses initialise state and later ones tidy up. Guard clauses (`| -> |;`) protect finished results from being reprocessed.
- **Introduce markers.** Temporary symbols (`.`, `@`, `b`, `+`, `g`, etc.) turn the string into a miniature state machine. Plan your pipeline as phases and dedicate a few unique markers to each phase.
- **Add your own sentinels.** Don’t force callers to append `#` or `|`. Spend a few setup rules to add the sentinel automatically; it keeps programs composable and easier to test.
- **Reset after every pass.** Complex programs (like multiplication and unary→binary conversion) repeatedly consume part of the input and then restore the initial markers. This ensures that subsequent passes see the same structure.
- **Prove behaviour with traces.** Use `cabal run turing -- FILE --input STRING` while developing. The numbered trace quickly reveals where a pipeline stalls or loops.
- **Document what you learn.** When you discover a new trick or marker pattern, add a short note to this tutorial and cover it with a test. Future you (and teammates) will thank you.

## 13. Verifying and Iterating

Add new examples under `examples/` and extend `test/Main.hs` with deterministic checks (exact traces or final states) plus property tests when possible. Running `cabal test` recompiles the rules and fails fast if a documentation example goes stale.

Helpful commands:

```bash
cabal build                              # ensure the project still compiles
cabal test                               # run parser/CLI specs and tutorial examples
cabal run turing -- FILE --input STRING  # inspect traces interactively
cabal run turing -- FILE --input STRING --max-steps 20  # cap runaway traces
```

When you add new behaviours:

1. Drop a quick description and usage guidance into this tutorial.
2. Capture the final state (or trace) in a unit test so the documentation can never drift.
3. Note any reusable insights in `AGENTS.md` so the next change starts from a stronger baseline.

## 14. What to Try Next

- Implement unary addition that reuses the duplication pipeline as a subroutine.
- Extend the binary lookup table to cover wider inputs or derive it programmatically from a helper script.
- Explore optimisations such as using multiple guards to short-circuit different phases.

Happy rewriting!
