Per-module filtering: library dependency recompilation with transitive dependencies.

Library A depends on Library B. When B changes, Dune currently recompiles all
modules in the consuming stanza, even those that don't use A.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

Create library B (the base library):

  $ mkdir libB
  $ cat > libB/dune <<EOF
  > (library
  >  (name libB))
  > EOF

  $ cat > libB/libB.ml <<EOF
  > let base_value = 1000
  > EOF

  $ cat > libB/libB.mli <<EOF
  > val base_value : int
  > EOF

Create library A that depends on library B:

  $ mkdir libA
  $ cat > libA/dune <<EOF
  > (library
  >  (name libA)
  >  (libraries libB))
  > EOF

  $ cat > libA/a_impl.mli <<EOF
  > val get_base : unit -> int
  > val own_value : int
  > EOF

  $ cat > libA/a_impl.ml <<EOF
  > let get_base () = LibB.base_value
  > let own_value = 500
  > EOF

Create modules that use different parts of the dependency chain:

  $ cat > uses_a.ml <<EOF
  > let call_a () = LibA.A_impl.own_value
  > EOF

  $ cat > uses_a.mli <<EOF
  > val call_a : unit -> int
  > EOF

  $ cat > independent.ml <<EOF
  > let standalone () = 999
  > EOF

  $ cat > independent.mli <<EOF
  > val standalone : unit -> int
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (modules main uses_a independent)
  >  (libraries libA))
  > EOF

  $ cat > main.ml <<EOF
  > let () =
  >   print_int (Uses_a.call_a ());
  >   print_int (Independent.standalone ())
  > EOF

Build first:

  $ dune build ./main.exe

Now change libB's interface (the transitive dependency):

  $ cat > libB/libB.mli <<EOF
  > val base_value : int
  > val new_base_fn : unit -> string
  > EOF

  $ cat > libB/libB.ml <<EOF
  > let base_value = 1000
  > let new_base_fn () = "new"
  > EOF

Independent is no longer recompiled because it doesn't reference libA or libB:

  $ dune build ./main.exe --display short 2>&1 | grep Independent
  [1]
