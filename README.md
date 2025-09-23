# Turing Rules Playground

Turing is a tiny playground for experimenting with the string rewriting language **Rules**. A Rules program is a list of rewrite clauses that are applied from left to right to transform an input string. This repository provides:

- A command line executable that parses a rules file and either runs an interactive Read–Eval–Print loop (REPL) or traces a single input.
- A growing library of example programs under [`examples/`](examples/).
- A comprehensive from-zero-to-hero tutorial in [`tutorial.md`](tutorial.md).
- A battery of automated tests that keep the documentation examples up to date (`cabal test`).

## Prerequisites

You need the Glasgow Haskell Compiler (GHC) and Cabal. The project is tested with GHC 9.12.2. Install the [Haskell toolchain](https://www.haskell.org/downloads/), then run:

```bash
cabal update
cabal build
```

## Running the Executable

The executable expects the path to a `.rules` file. The optional `--input` flag lets you trace a single string. Without `--input`, the program drops into an interactive REPL where every line you type is traced.

```bash
cabal run turing -- RULES-FILE [--input STRING] [--max-steps N]
```

- `RULES-FILE` – path to a file containing Rules clauses.
- `--input STRING` – run once on `STRING` and exit instead of launching the REPL.
- `--max-steps N` – truncate the trace after `N` rewrite steps (0 = unlimited). The same limit is applied to REPL traces when `--input` is omitted.

### Interactive REPL shortcuts

When the REPL is active, a short banner lists the available shortcuts:

- Press <kbd>Ctrl</kbd>+<kbd>R</kbd> (or type `:reload`) to parse the rules file again.
- Press <kbd>Ctrl</kbd>+<kbd>S</kbd> (or type `:steps`) and enter a natural number to set the maximum rewrite steps for future traces. Enter `0` to remove the cap.

The current step limit is echoed after every change, and the REPL prints a reminder whenever a trace stops early because it hit the configured limit.

### Example: Appending a Guard

The [`examples/append-bar.rules`](examples/append-bar.rules) program appends a terminal guard (`|`) to any unary string. Tracing `111` yields the following steps (asserted by the test suite):

```text
step 0: 111
step 1: .111
step 2: 1.11
step 3: 11.1
step 4: 111.
step 5: 111|
```

Reproduce the run yourself:

```bash
cabal run turing -- examples/append-bar.rules --input 111
```

The program first prints the parsed rules and then lists the numbered steps shown above. Add `--max-steps` if you want to cap exploratory runs while drafting new rules, or press <kbd>Ctrl</kbd>+<kbd>S</kbd> inside the REPL to adjust the limit on the fly.

## Examples Library

Each example has a matching walkthrough in the tutorial and is verified by `cabal test`.

- [`examples/append-bar.rules`](examples/append-bar.rules) – introduce a cursor to append a guard bar (tutorial §3).
- [`examples/duplicate.rules`](examples/duplicate.rules) – duplicate a unary payload without requiring sentinels (tutorial §4).
- [`examples/unary-multiply.rules`](examples/unary-multiply.rules) – perform unary multiplication with staged pointers (tutorial §5).
- [`examples/unary-to-binary.rules`](examples/unary-to-binary.rules) – convert unary counts into binary digits via an incrementing workspace (tutorial §6).
- [`examples/binary-to-unary.rules`](examples/binary-to-unary.rules) – map binary numerals back to unary using a lookup table (tutorial §7).
- [`examples/binary-increment.rules`](examples/binary-increment.rules) – increment binary strings while preserving a guard (tutorial §8).

Try composing them: unary → binary → increment → binary → unary yields a unary successor machine powered entirely by the provided examples.

## Testing

Run all tests—including the documentation examples—with:

```bash
cabal test
```

The suite exercises the parser, CLI, REPL formatting, and every example in `examples/`. If you change a rules file or introduce a new tutorial snippet, add a corresponding test so the docs remain trustworthy.

## Learning the Rules Language

Start with [`tutorial.md`](tutorial.md) for the guided tour of syntax, semantics, strategies, and composition patterns. Every time you add a new example or discover a fresh trick, update the tutorial and its companion tests so future readers inherit the full story.
