# `feat:` review concerns

When reviewing a `feat:` PR, apply these concerns in addition to those in
`general.md`.

## Concerns specific to `feat:`

- **Version guard.** New stanzas, fields, actions, or any behaviour reaching
  users must be guarded with `Dune_lang.Syntax.since` against the relevant
  `(lang dune X.Y)`.

- **Experimental features.** For features that can't guarantee strict backwards
  compatibility, define an independent language via `Dune_lang.Syntax.create
  ~name:... ~experimental:true ...` with versions below 1.0; namespace
  stanzas/fields as `ext_name.stanza_name`.

- **Validate user input eagerly.** New pforms, stanzas, or identifiers must
  reject invalid values at parse time with good error messages, not deferred
  until later.

- **Register file paths as action dependencies** so sandboxed and incremental
  builds stay correct.

- **Don't silently drop threaded values.** Every place that ignores
  `env`/deps/errors must be deliberate and commented.

- **Tests demonstrating the new behaviour** — cram or expect. Cover failure
  paths, not just the happy case.

- **Watch mode.** Any new path that observes files must go through `Fs_memo`.

- **Warnings via `Dune_lang.Warning`** so users can disable them.

See `doc/hacking.rst` § *Adding Stanzas* for the implementation workflow: extend
`Stanza.t` → modify `Dune_file` parsing → modify `Gen_rules`.
