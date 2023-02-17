Reproduce a bug where an implementation's module archive isn't correctly
constructed. A virtual module's implementation can be excluded from the
archive.

https://github.com/ocaml/dune/issues/7027

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > EOF
  $ mkdir vlib
  $ cat >vlib/dune <<EOF
  > (library
  >  (name vlib)
  >  (wrapped false)
  >  (virtual_modules x))
  > EOF
  $ cat >vlib/a.ml <<EOF
  > let f = X.make ()
  > EOF
  $ cat >vlib/x.mli <<EOF
  > type t
  > val make : unit -> t
  > EOF

  $ mkdir impl
  $ cat >impl/dune <<EOF
  > (library
  >  (name impl)
  >  (implements vlib))
  > EOF
  $ cat >impl/x.ml <<EOF
  > type t = unit
  > let make () = ()
  > EOF
  $ cat >impl/z.ml <<EOF
  > let g = A.f
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (modes byte)
  >  (name foo)
  >  (libraries impl))
  > EOF

  $ cat >foo.ml <<EOF
  > let _ = X.make ()
  > EOF

  $ dune exec ./foo.exe
