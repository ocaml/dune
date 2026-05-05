A consumer library that uses [(root_module ...)] to alias one of
its dependencies is rebuilt when the dependency's interface
changes, and not rebuilt when only the dependency's
implementation changes. This locks in the incremental-rebuild
property for [Root]-aliased dependencies that any future change
to dune's inter-library-dependency tracking must preserve.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ mkdir dep_lib consumer_lib
  $ cat > dep_lib/dune <<EOF
  > (library (name dep_lib))
  > EOF
  $ cat > dep_lib/dep_lib.ml <<EOF
  > let extra = 0
  > let greeting = "hello-" ^ string_of_int extra
  > EOF
  $ cat > dep_lib/dep_lib.mli <<EOF
  > val greeting : string
  > EOF

  $ cat > consumer_lib/dune <<EOF
  > (library
  >  (name consumer_lib)
  >  (libraries dep_lib)
  >  (root_module root))
  > EOF
  $ cat > consumer_lib/consumer_lib.ml <<EOF
  > let () = print_endline Root.Dep_lib.greeting
  > EOF

  $ dune build @check

Editing only [dep_lib.mli] (the [.ml] is unchanged) changes
[dep_lib.cmi] and must invalidate [consumer_lib]:

  $ cat > dep_lib/dep_lib.mli <<EOF
  > val greeting : string
  > val extra : int
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("\\.consumer_lib\\.objs/"))]'
  [
    {
      "target_files": [
        "_build/default/consumer_lib/.consumer_lib.objs/byte/consumer_lib__Root.cmi",
        "_build/default/consumer_lib/.consumer_lib.objs/byte/consumer_lib__Root.cmo",
        "_build/default/consumer_lib/.consumer_lib.objs/byte/consumer_lib__Root.cmt"
      ]
    },
    {
      "target_files": [
        "_build/default/consumer_lib/.consumer_lib.objs/byte/consumer_lib.cmi",
        "_build/default/consumer_lib/.consumer_lib.objs/byte/consumer_lib.cmo",
        "_build/default/consumer_lib/.consumer_lib.objs/byte/consumer_lib.cmt"
      ]
    }
  ]

Editing only [dep_lib.ml] (no [.mli] change) leaves [dep_lib.cmi]
untouched, so [consumer_lib] is not rebuilt:

  $ cat > dep_lib/dep_lib.ml <<EOF
  > let extra = 1
  > let greeting = "hello-" ^ string_of_int extra
  > EOF
  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("\\.consumer_lib\\.objs/"))]'
  []
