# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
the Dune codebase.

## Quick Reference

**Most Common Commands:**
```bash
dune build @check          # Quick build (recommended for development)
dune runtest dir/           # Run tests in specific directory
dune fmt                   # Auto-format code (always run before committing)
dune promote               # Accept test output changes (ask user first)
make dev                   # Full build (bootstraps automatically if needed)
```

**Special Operations (Ask User First):**
```bash
make bootstrap             # Rebuild entire toolchain from scratch
```

Note: `dune` refers to `./dune.exe` (the bootstrapped version).

## Project Overview

Dune is a self-hosting OCaml build system that uses itself to build itself.

**Key Concepts:**
- **Bootstrap**: Building dune from scratch using `make bootstrap` (ask user
  first)
- **Cram Tests**: `.t` files containing shell commands and expected outputs
- **Test Promotion**: Accepting new test outputs when behavior changes
- **Self-hosting**: Dune builds itself using a previously built version

## Architecture

**Directory Structure:**
- `bench` - performance benchmarks
- `bin` - dune's command line interface
- `boot` - bootstrap mechanism for building dune itself
- `doc` - user documentation
- `otherlibs` - public libraries (dune-configurator, dune-build-info, etc.)
- `src` - the majority of the source code
  - `src/dune_rules` - build rule generation (main logic)
  - `src/dune_engine` - incremental build engine
  - `src/dune_lang` - configuration file parser
  - `src/dune_pkg` - package management
  - `src/fiber` - async/concurrency library
  - `src/stdune` - dune's standard library
  - `src/dune_tui` - terminal UI components
- `test` - dune's test suite (`.t` files are cram tests)
- `vendor` - 3rd party code pulled into dune

## Development Workflow

### Build Commands
```bash
dune build @check          # Quick build (recommended for development)
dune build @install        # Full build
dune fmt                   # Auto-format code (always run before committing)
```

### Bootstrap Process

**What it is:** Bootstrap solves Dune's circular dependency (Dune builds Dune)
using `boot/bootstrap.ml` - a mini-build system that creates `_boot/dune.exe`
without reading any dune files.

**When needed:**
- Fresh repository checkout (no `_boot/dune.exe` exists)
- Changes to core build system dependencies in `boot/libs.ml`
- After certain clean operations that remove `_boot/`

**Why ask user first:** Bootstrap rebuilds the entire toolchain from scratch
using a carefully orchestrated process. Most development uses the existing
`_boot/dune.exe`.

**When NOT to bootstrap:** For normal development work, use `dune build @check`
or `make dev`. Bootstrap is only needed for the specific circumstances above.

**Commands:**
- `make bootstrap` - Full bootstrap rebuild (ask user first)
- `make test-bootstrap` - Test bootstrap mechanism
- `make dev` - Automatically bootstraps only if necessary

### Test Commands
```bash
dune runtest dir/           # Run tests in specific directory
dune runtest dir/test.t     # Run specific .t test (cram test)
dune runtest                # Run all tests (567+ tests, very slow)
```

**Output Handling:** Dune is generally silent when building and only outputs
errors. Avoid truncating output from `dune build` and `dune runtest`. If
`dune runtest` gives too much output, run something of smaller scope instead.

**Test Promotion:** When tests fail due to output changes, ask user before
running `dune promote` to accept changes.

**Experimentation:** Create cram tests (`.t` files) to experiment with how
things work. Don't run commands manually - run them through `dune runtest` to
capture and verify behavior.

**Printf Debugging:** When confused about behavior, use `Dune_console`
(commonly aliased as `Console`) for debugging:
```ocaml
Console.printf "something: %s" (Something.to_dyn something |> Dyn.to_string);
```
This output will appear in cram test diffs, making it easy to observe values.

### Development Guidelines
- Always verify changes build with `dune build @check`
- Run `dune fmt` to ensure code formatting (requires ocamlformat)
- Keep lines under 80 characters
- Only add comments for complex algorithms or when explicitly requested
- Don't disable warnings or tests unless prompted
- Use pattern-matching and functional programming idioms
- Avoid `assert false` and other unreachable code

## Code Conventions

### OCaml Patterns
- Every `.ml` file needs corresponding `.mli` (except type-only files)
- Use `Code_error.raise` instead of `assert false` for better error messages
- Qualify record construction: `{ Module.field = value }`
- Prefer destructuring over projection: `let { Module.field; _ } = record` not
  `record.Module.field`
- Pattern match exhaustively in `to_dyn` functions: `let to_dyn {a; b; c} = ...`

## Critical Constraints

**NEVER do these things:**
- NEVER create files unless absolutely necessary
- NEVER proactively create documentation files (*.md) or README files
- NEVER stage or commit changes unless explicitly requested
- NEVER run `dune clean`
- NEVER use the `--force` argument
- NEVER try to build dune manually to run a test

**ALWAYS do these things:**
- ALWAYS prefer editing existing files over creating new ones
- ALWAYS ask user before running `dune promote` or `make bootstrap`