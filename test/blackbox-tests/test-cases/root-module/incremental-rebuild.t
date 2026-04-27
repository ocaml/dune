A consumer library that uses [(root_module ...)] to alias one of
its dependencies is rebuilt when the dependency's interface
changes, and not rebuilt when only the dependency's
implementation changes. This locks in the incremental-rebuild
property for [Root]-aliased dependencies that any future change
to dune's inter-library-dependency tracking must preserve.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ mkdir lib1 lib2
  $ cat > lib1/dune <<EOF
  > (library (name lib1))
  > EOF
  $ cat > lib1/lib1.ml <<EOF
  > let extra = 0
  > let greeting = "hello-" ^ string_of_int extra
  > EOF
  $ cat > lib1/lib1.mli <<EOF
  > val greeting : string
  > EOF

  $ cat > lib2/dune <<EOF
  > (library (name lib2) (libraries lib1) (root_module root))
  > EOF
  $ cat > lib2/lib2.ml <<EOF
  > let () = print_endline Root.Lib1.greeting
  > EOF

  $ dune build @check

Editing only [lib1.mli] (the [.ml] is unchanged) changes
[lib1.cmi] and must invalidate [lib2]:

  $ cat > lib1/lib1.mli <<EOF
  > val greeting : string
  > val extra : int
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("\\.lib2\\.objs/"))] | length > 0'
  true

Editing only [lib1.ml] (no [.mli] change) leaves [lib1.cmi]
untouched, so [lib2] is not rebuilt:

  $ cat > lib1/lib1.ml <<EOF
  > let extra = 1
  > let greeting = "hello-" ^ string_of_int extra
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("\\.lib2\\.objs/"))] | length'
  0
