Incremental build is broken when a consumer uses the old (compat) module
names from a library with (wrapped (transition ...)).

A library migrating from unwrapped to wrapped exposes compat shims so that
downstream code can keep using the bare module name (Foo) instead of the
new qualified name (Mylib.Foo). 

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ mkdir mylib
  $ cat > mylib/dune <<EOF
  > (library
  >  (name mylib)
  >  (wrapped
  >   (transition "Use Mylib.Foo instead")))
  > EOF

  $ cat > mylib/mylib.ml <<EOF
  > module Foo = Foo
  > EOF

  $ cat > mylib/foo.ml <<EOF
  > type t = { x : int }
  > let v : t = { x = 1 }
  > EOF

  $ cat > mylib/foo.mli <<EOF
  > type t = { x : int }
  > val v : t
  > EOF

A consumer still using the bare name during the migration:

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (libraries mylib)
  >  (ocamlopt_flags (:standard -alert -deprecated))
  >  (ocamlc_flags (:standard -alert -deprecated)))
  > EOF
  $ cat > main.ml <<EOF
  > let () = print_int Foo.v.Foo.x
  > EOF

  $ dune build ./main.exe

The compat shim foo.cmi now depends on both the wrapper mylib.cmi and the
inner module mylib__Foo.cmi that it re-exports:

  $ dune rules --deps mylib/.mylib.objs/byte/foo.cmi 2>&1 | grep In_build_dir
   (File (In_build_dir _build/default/mylib/.mylib.objs/byte/mylib.cmi))
   (File (In_build_dir _build/default/mylib/.mylib.objs/byte/mylib__Foo.cmi))
   (File (In_build_dir _build/default/mylib/.wrapped_compat/Foo.ml-gen)))

The library author adds a field:

  $ cat > mylib/foo.ml <<EOF
  > type t = { x : int; y : int }
  > let v : t = { x = 1; y = 2 }
  > EOF
  $ cat > mylib/foo.mli <<EOF
  > type t = { x : int; y : int }
  > val v : t
  > EOF

The incremental rebuild succeeds:

  $ dune build ./main.exe
