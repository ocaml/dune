A consumer transitively depends upon a library through one module of an
intermediate library, never naming a sibling module of the transitive lib in
source. The current but undesirable behaviour is that the consumer rebuilds
when any module of the transitively depended upon library changes, even
unreferenced ones.

[intermediate_lib] and [main] each include an unused dummy module so neither
stanza is single-module — single-module stanzas take a fast path that would
mask the behaviour being baselined here.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ cat > dune <<EOF
  > (library (name dep_lib) (wrapped false) (modules reached_module unreached_module))
  > (library (name intermediate_lib) (wrapped false) (modules intermediate_module intermediate_dummy) (libraries dep_lib))
  > (executable (name main) (modules main main_dummy) (libraries intermediate_lib))
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
  $ cat > intermediate_dummy.ml <<EOF
  > let _ = ()
  > EOF
  $ cat > main.ml <<EOF
  > let _ = Intermediate_module.v
  > EOF
  $ cat > main_dummy.ml <<EOF
  > let _ = ()
  > EOF

  $ dune build ./main.exe

Edit [unreached_module] (both [.mli] and [.ml]) — a module no source in this
test references — and record the rebuild list for [main]:

  $ cat > unreached_module.mli <<EOF
  > val v : int
  > val extra : int
  > EOF
  $ cat > unreached_module.ml <<EOF
  > let v = 43
  > let extra = 99
  > EOF
  $ dune build ./main.exe
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("dune__exe__Main\\."))]'
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
