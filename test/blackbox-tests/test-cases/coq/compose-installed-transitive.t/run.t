Tests for meta-data for installed theories built with Dune. c.f.

- Coq cannot find the cmxs file for transitive ocaml dependencies (https://github.com/ocaml/dune/issues/11012)
- Dune-Coq: support transitive dependencies even for installed theories (https://github.com/ocaml/dune/issues/11483)

In particular, we will build a set of theories that depend on each
other, then install them. Then our user library will depend on the
outer one, and dune should realize the transitive dependencie (both
for .vo and ML plugins)

  $ dune build --root to_install @all
  Entering directory 'to_install'
  Hello
  Hello
  Hello
  Hello
  Leaving directory 'to_install'
  $ dune install --root to_install --prefix=$PWD

We now build the normal theory, and should work

  $ COQPATH=$PWD/lib/coq/user-contrib dune build --root user @all
  Entering directory 'user'
  *** Warning: in file bar.v, library bar is required from root T_a and has not been found in the loadpath!
  *** Warning: in file bar.v, library bar is required from root T_b and has not been found in the loadpath!
  *** Warning: in file bar.v, library bar is required from root T_c and has not been found in the loadpath!
  File "./bar.v", line 1, characters 0-28:
  Error: Cannot find a physical path bound to logical path bar with prefix T_a.
  
  Leaving directory 'user'
  [1]
