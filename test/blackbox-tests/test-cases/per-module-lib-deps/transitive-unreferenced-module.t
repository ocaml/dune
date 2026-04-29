A consumer reaches a transitive dep library through one module
of an intermediate library, never naming a sibling module of
the transitive lib in source. Today the consumer rebuilds when
any module of the transitive lib changes, even unreferenced
ones.

Setup. [dep_lib] is unwrapped with two modules: [reached_module]
and [unreached_module]; nothing references [unreached_module]
from outside [dep_lib]. [intermediate_lib] depends on [dep_lib]
and references only [Reached_module] from it. [main] depends on
[intermediate_lib] and references only [Intermediate_module];
[main]'s source never names [unreached_module].

This test records [main]'s current rebuild list so a future
per-module filter can flip it to empty.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ cat > dune <<EOF
  > (library (name dep_lib) (wrapped false) (modules reached_module unreached_module))
  > (library (name intermediate_lib) (wrapped false) (modules intermediate_module) (libraries dep_lib))
  > (executable (name main) (modules main) (libraries intermediate_lib))
  > EOF

  $ cat > reached_module.ml <<EOF
  > let v = 42
  > EOF
  $ cat > reached_module.mli <<EOF
  > val v : int
  > EOF
  $ cat > unreached_module.ml <<EOF
  > let v = 43
  > EOF
  $ cat > unreached_module.mli <<EOF
  > val v : int
  > EOF
  $ cat > intermediate_module.ml <<EOF
  > let v = Reached_module.v
  > EOF
  $ cat > main.ml <<EOF
  > let _ = Intermediate_module.v
  > EOF

  $ dune build ./main.exe

Edit [unreached_module] (both [.mli] and [.ml]) — a module no
source in this test references — and record the rebuild list for
[main]:

  $ cat > unreached_module.mli <<EOF
  > val v : int
  > val extra : int
  > EOF
  $ cat > unreached_module.ml <<EOF
  > let v = 43
  > let extra = 99
  > EOF
  $ dune build ./main.exe
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("dune__exe__Main"))]'
  [
    {
      "target_files": [
        "_build/default/.main.eobjs/byte/dune__exe__Main.cmi",
        "_build/default/.main.eobjs/byte/dune__exe__Main.cmti"
      ]
    },
    {
      "target_files": [
        "_build/default/.main.eobjs/native/dune__exe__Main.cmx",
        "_build/default/.main.eobjs/native/dune__exe__Main.o"
      ]
    }
  ]
