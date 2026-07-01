# `refactor:` review concerns

When reviewing a `refactor:` PR, apply these concerns in addition to those in
`general.md`.

## Concerns specific to `refactor:`

- **No behaviour change** is the load-bearing claim. Scrutinise library-helper
  swaps for default-argument semantics that diverge from the open-coded version
  (e.g. `Fpath.traverse` raising vs ignoring on FIFOs/sockets).

- **Cache lifetime.** Any new `Memo.create` / `Table` needs an explicit lifetime
  story under `--watch` and RPC.

- **Bump digest/cache versions** when the on-disk representation changes;
  otherwise stale caches will silently misbehave.

- **Local-name shadowing.** Inner `let` bindings that shadow module-level
  functions should be renamed.
