# `test:` review concerns

When reviewing a `test:` PR, apply these concerns in addition to those in
`general.md`.

## Concerns specific to `test:`

- **Pin properties via traces, not surface output.** Prefer `dune trace cat |
  jq` or `dune rules --deps` over `--display short`, mtimes, or full action-list
  snapshots — UI/scheduling changes shouldn't break the test. Use helpers from
  the shared jq library at `test/blackbox-tests/dune.jq` (e.g. `jq 'include
  "dune"; processes'`). Enable extra trace categories with `DUNE_TRACE=cache`
  (full list in `src/dune_trace/category.ml`).

- **Fixture must exercise the claimed path.** Singleton-module libraries skip
  ocamldep; `@check` is gated on merlin — verify the setup actually trips the
  bug.

- **Filter output to the property under test**, not the full event stream or
  action list.

- **Use existing test helpers**: `dune_cmd wait-for-file-to-appear`,
  `make_dune_project`, `censor` (replaces hex digests with stable
  `$DIGEST1`/`$DIGEST2` labels — distinct inputs get distinct labels), percent
  forms over magic paths.

- **Portability.** No GNU-only flags (`stat -c`); no sleep-based waits; mark
  Windows-incompatible cases explicitly with `(enabled_if ...)`.

- **Use current `(lang dune X.Y)`** unless the test specifically asserts
  back-compat behaviour.

- **Semantic fixture names.** `spurious_rebuild.ml` over `modB1`.

- **Self-describing prose at the top** — one or two sentences, accurate.

- **No stack traces** in expected output — they are version-sensitive.

- **`INSIDE_DUNE` awareness.** Dune sets this when running inside tests; it
  changes root detection (CWD over `dune-project`), quiets messages, and makes
  error output deterministic. Don't fight this — let it work.
