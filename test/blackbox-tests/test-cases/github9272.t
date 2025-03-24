Repro for #9272: when an executable that depends on dune-site is promoted to
the source tree, the executable in the source tree segfaults.

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > (using dune_site 0.1)
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (name hello)
  >  (promote (until-clean))
  >  (libraries dune-site))
  > EOF

  $ touch hello.ml

  $ dune build

  $ ./hello.exe
