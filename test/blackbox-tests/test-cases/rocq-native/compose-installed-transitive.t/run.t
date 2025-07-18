Tests for meta-data for installed theories built with Dune. c.f.

- Coq cannot find the cmxs file for transitive ocaml dependencies (https://github.com/ocaml/dune/issues/11012)
- Dune-Coq: support transitive dependencies even for installed theories (https://github.com/ocaml/dune/issues/11483)

In particular, we will build a set of theories that depend on each
other, then install them. Then our user library will depend on the
outer one, and dune should realize the transitive dependencie (both
for .vo and ML plugins)

  $ dune build --root to_install @all
  Entering directory 'to_install'
  File "./theory_a/bar.v", line 1, characters 0-41:
  Warning:
  Legacy loading plugin method has been removed from Rocq, and the `:` syntax is deprecated, and its first argument ignored; please remove "plugin:" from your Declare ML
  [legacy-loading-removed,deprecated-since-9.0,deprecated,default]
  Hello
  Hello
  Hello
  Hello
  Leaving directory 'to_install'
  $ dune install --root to_install --prefix=$PWD

We now build the normal theory, and should work

  $ ROCQPATH=$PWD/lib/coq/user-contrib dune build --root user @all
  Entering directory 'user'
  File "dune", line 4, characters 11-14:
  4 |  (theories T_d))
                 ^^^
  Theory "T_d" has not been found.
  -> required by theory User in dune:2
  -> required by _build/default/.User.theory.d
  -> required by alias all
  Leaving directory 'user'
  [1]
