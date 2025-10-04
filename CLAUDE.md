# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

# Dune Development Guide

## Key Architecture

**Directory Structure:**
- `bench` - contain benchmarks
- `bin` - dune's command line
- `boot` - building dune itself
- `doc` - documentation
- `otherlibs` - public libraries that live in this repository. not necessarily dune related
- `src` - the majority of the source code
  - `src/dune_rules` - build rule generation (main logic)
  - `src/dune_engine` - incremental build engine
  - `src/dune_lang` - configuration file parser
- `test` - dune's test suite
- `vendor` - 3rd party code pulled into dune.

## Build Commands
```bash
make check                 # Quick build (recommended for development)
make dev                   # Full build
make fmt                   # Auto-format code (always run before committing)
```

## Test Commands
```bash
dune runtest dir/test.t # To run a .t test (each .t is a full dune project)
dune promote dir/test.t # To accept test output changes
make test # To run all tests (567+ tests, very slow)
```

## Development Guidelines
- Always verify changes build with `make check`
- Run `make fmt` to ensure code formatting (requires ocamlformat 0.27.0)
- Keep lines under 80 characters
- Don't add excessive comments unless prompted
- Don't disable warnings or tests unless prompted
- Use pattern-matching and functional programming idioms
- Avoid `assert false` and other unreachable code

## Important Notes

- NEVER create files unless absolutely necessary
- ALWAYS prefer editing existing files
- NEVER proactively create documentation files (*.md) or README files
- NEVER stage or commit changes unless explicitly requested
- NEVER run `dune clean`
- NEVER use the `--force` argument
- NEVER try to build dune manually to run a test
