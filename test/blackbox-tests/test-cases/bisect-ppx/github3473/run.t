  $ cat >dune-project <<EOF
  > (lang dune 2.5)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name hello)
  >  (bisect_ppx))
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 2.6)
  > 
  > (context default)
  > (context (default (name coverage) (bisect_enabled true)))
  > EOF

  $ dune build @all 2>&1 | grep -v 'file "'
  File "dune", line 3, characters 1-13:
  3 |  (bisect_ppx))
       ^^^^^^^^^^^^
  Error: 'bisect_ppx' is disabled. Enable it in your dune-project file
