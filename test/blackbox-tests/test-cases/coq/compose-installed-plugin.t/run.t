Test for https://github.com/ocaml/dune/issues/7893
When using an installed theory with plugins, things should work fine.

  $ dune build --root to_install @all
  Entering directory 'to_install'
  Hello
  Leaving directory 'to_install'
  $ dune install --root to_install --prefix=$PWD

We now build the normal theory, and should work

  $ COQPATH=$PWD/lib/coq/user-contrib dune build --root user @all
  Entering directory 'user'
  Hello
  Leaving directory 'user'
