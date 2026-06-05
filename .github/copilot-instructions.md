# Copilot Instructions

For full development guidance, see [AGENTS.md](../AGENTS.md).

## Environment Setup

This repository uses a [Nix](https://nixos.org/) dev shell to provide the
OCaml toolchain (`ocaml`, `dune`, `opam`, `ocamlformat`, etc.).  The shell is
**not** activated automatically, so every command must be prefixed with
`nix develop -c`.

### First-time setup

On a fresh checkout (or after the `_boot/` directory has been removed), run
bootstrap once before anything else:

```bash
nix develop -c make bootstrap
```

This builds `_boot/dune.exe`, the seed binary that Dune uses to build itself.
It is slow but only needed once per environment.

## Common Commands

All commands from [AGENTS.md](../AGENTS.md) must be wrapped with
`nix develop -c`:

```bash
nix develop -c dune build @check          # Quick build (recommended)
nix develop -c dune build @install        # Full build
nix develop -c dune runtest dir/          # Run tests in a directory
nix develop -c dune runtest dir/test.t    # Run a specific cram test
nix develop -c dune fmt                   # Auto-format code (run before committing)
nix develop -c dune promote               # Accept test output changes (ask user first)
nix develop -c make dev                   # Full build (bootstraps if necessary)
```

## Reviewing PRs

See [doc/dev/code-review/general.md](../doc/dev/code-review/general.md) and
follow the instructions there.

For high-stakes reviews (public API changes, security-sensitive code, large
PRs), prefer a two-pass workflow:

1. A first agent drafts the review against `general.md`'s guidance.
2. A second, fresh-context agent (no prior conversation with the drafter)
   reads the draft and applies the "Validation pass" criteria from
   `general.md`.

The validator must not be the drafter — the value comes from independent
judgement. Use whichever multi-agent primitive your harness provides
(subagents, separate sessions, parallel runs).
