- For `(lang dune 3.24)` or newer with `(generate_opam_files true)`,
  the generated opam file now enforces the menhir lower bound
  (`>= "20180523"`) that dune's menhir rules require, even when the
  package already lists a menhir constraint. A warning is emitted when
  the user's lower bound is below the minimum. `(lang dune 3.23)`
  projects keep the pre-existing behaviour: bare-`(menhir)` deps still
  get the lower bound filled in (from #14434), but user-written menhir
  constraints pass through unchanged. `(lang dune 3.22)` and earlier
  remain entirely untouched. The tiered gate mirrors #14466 to avoid
  silent changes to opam output on dune-binary upgrade.
  (#14453, @robinbb)
