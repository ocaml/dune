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
  Error: 'bisect_ppx' is available only when bisect_ppx is enabled in the
  dune-project file. It cannot be enabled automatically because the currently
  selected version of dune (2.5) does not support this plugin.
  You must enable it using (using bisect_ppx ..) in your dune-project file. The
  first version of this plugin 1.0 was introduced in dune 2.6.
