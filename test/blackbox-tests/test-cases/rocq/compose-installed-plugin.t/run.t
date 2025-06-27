Test for https://github.com/ocaml/dune/issues/7893
When using an installed theory with plugins, things should work fine.

  $ dune build --root to_install @all
  Entering directory 'to_install'
  File "./theory/bar.v", line 1, characters 0-41:
  Warning:
  Legacy loading plugin method has been removed from Rocq, and the `:` syntax is deprecated, and its first argument ignored; please remove "plugin:" from your Declare ML
  [legacy-loading-removed,deprecated-since-9.0,deprecated,default]
  Hello
  Leaving directory 'to_install'
  $ dune install --root to_install --prefix=$PWD

We now build the normal theory, and should work

  $ OCAMLPATH=$PWD/lib/:$OCAMLPATH
  $ COQPATH=$PWD/lib/coq/user-contrib dune build --root user @all
  Entering directory 'user'
  Hello
  Leaving directory 'user'
