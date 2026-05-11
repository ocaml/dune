- When generating opam files for `(lang dune 3.23)` or newer, AND
  the user-written menhir dep constraint with the lower bound
  (`>= "20180523"`) that dune's menhir rules require. The merge
  descends recursively through `(and ...)` / `(or ...)` /
  `(not ...)` to find existing `>=` literals, tightening any that
  fall below the minimum (with a user-facing warning), and skips
  the outer AND when the resulting expression already implies the
  minimum on every satisfying valuation. The same recursive merge
  is applied to dune-on-dune constraints. The 3.23 gate also
  covers the bare-`(menhir)` fill from #14434 (previously
  unversioned), mirroring the fix in #14466 to avoid silent
  changes to opam output on dune-binary upgrade.
  (#14453, @robinbb)
