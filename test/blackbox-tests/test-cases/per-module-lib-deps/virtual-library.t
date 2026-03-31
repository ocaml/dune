Baseline: library dependency recompilation with virtual libraries.

When a virtual library's interface changes, Dune currently recompiles all
modules in stanzas that depend on it, even those that don't reference it.

See: https://github.com/ocaml/dune/issues/4572

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

Create a virtual library:

  $ mkdir vlib
  $ cat > vlib/dune <<EOF
  > (library
  >  (name vlib)
  >  (virtual_modules vmod))
  > EOF

  $ cat > vlib/vmod.mli <<EOF
  > val virtual_fn : int -> int
  > EOF

Create an implementation of the virtual library:

  $ mkdir vlib_impl
  $ cat > vlib_impl/dune <<EOF
  > (library
  >  (name vlib_impl)
  >  (implements vlib))
  > EOF

  $ cat > vlib_impl/vmod.ml <<EOF
  > let virtual_fn x = x * 3
  > EOF

Create modules that use or don't use the virtual library:

  $ cat > uses_vlib.ml <<EOF
  > let call_vlib x = Vlib.Vmod.virtual_fn x
  > EOF

  $ cat > uses_vlib.mli <<EOF
  > val call_vlib : int -> int
  > EOF

  $ cat > no_vlib.ml <<EOF
  > let no_virtual () = 42
  > EOF

  $ cat > no_vlib.mli <<EOF
  > val no_virtual : unit -> int
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (modules main uses_vlib no_vlib)
  >  (libraries vlib_impl))
  > EOF

  $ cat > main.ml <<EOF
  > let () =
  >   print_int (Uses_vlib.call_vlib 10);
  >   print_int (No_vlib.no_virtual ())
  > EOF

Build first:

  $ dune build ./main.exe

Now change the virtual library's interface:

  $ cat > vlib/vmod.mli <<EOF
  > val virtual_fn : int -> int
  > val new_virtual : string -> string
  > EOF

Update the implementation to match:

  $ cat > vlib_impl/vmod.ml <<EOF
  > let virtual_fn x = x * 3
  > let new_virtual s = s ^ "!"
  > EOF

No_vlib is recompiled even though it doesn't reference the virtual library:

  $ dune build ./main.exe --display short 2>&1 | grep No_vlib
        ocamlc .main.eobjs/byte/dune__exe__No_vlib.{cmi,cmti}
      ocamlopt .main.eobjs/native/dune__exe__No_vlib.{cmx,o}
