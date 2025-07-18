Test for https://github.com/ocaml/dune/issues/7893
When using an installed theory with plugins, things should work fine.

  $ dune build --root to_install @all
  Entering directory 'to_install'
  Hello
  Leaving directory 'to_install'
  $ dune install --root to_install --prefix=$PWD

We now build the normal theory, and should work

  $ OCAMLPATH=$PWD/lib/:$OCAMLPATH
  $ COQPATH=$PWD/lib/coq/user-contrib dune build --root user @all
  Entering directory 'user'
  Warning, feedback message received but no listener to handle it!
  Warning: Deprecated environment variable COQPATH, use ROCQPATH instead.
  [deprecated-coq-env-var,deprecated-since-9.0,deprecated,default]
  Warning, feedback message received but no listener to handle it!
  Warning: Deprecated environment variable COQPATH, use ROCQPATH instead.
  [deprecated-coq-env-var,deprecated-since-9.0,deprecated,default]Hello
  Leaving directory 'user'
