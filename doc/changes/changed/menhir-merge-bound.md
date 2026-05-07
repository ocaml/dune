- When generating opam files for `(lang dune 3.23)` or newer, AND
  the user-written menhir dep constraint with the lower bound
  (`>= "20180523"`) that dune's menhir rules require. Take the
  higher of two `>=` literals (warn when the user's bound is below
  the minimum); combine with formula operators like `:with-test`.
  The 3.23 gate also covers the bare-`(menhir)` fill from #14434
  (previously unversioned), mirroring the fix in #14466 to avoid
  silent changes to opam output on dune-binary upgrade. Follows up
  #14434. (@robinbb)
