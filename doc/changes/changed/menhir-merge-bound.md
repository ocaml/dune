- For `(lang dune 3.23)` or newer with `(generate_opam_files true)`,
  the generated opam file now enforces the menhir lower bound
  (`>= "20180523"`) that dune's menhir rules require, even when the
  package already lists a menhir constraint. A warning is emitted when
  the user's lower bound is below the minimum. The 3.23 gate also
  covers the bare-`(menhir)` fill from #14434 (previously unversioned),
  mirroring #14466 to avoid silent changes to opam output on
  dune-binary upgrade.
  (#14453, @robinbb)
