Archive creation with unwrapped library and C stubs.
See https://github.com/ocaml/dune/issues/3766

  $ cat > dune-project << EOF
  > (lang dune 2.8)
  > (package
  >  (name foo))
  > EOF

  $ mkdir -p lib exe

  $ cat > lib/dune << EOF
  > (library
  >  (public_name foo)
  >  (wrapped false)
  >  (foreign_stubs (language c) (names stub))
  >  (modules ()))
  > EOF

  $ cat > lib/stub.c << EOF
  > void foo() {}
  > EOF

  $ cat > exe/dune << EOF
  > (executable
  >  (name b)
  >  (link_flags -linkall)
  >  (libraries foo))
  > EOF

  $ cat > exe/b.ml << EOF
  > let () = print_endline "exe working"
  > EOF

  $ dune build @install
  $ ls _build/install/default/lib/foo/*.a
  _build/install/default/lib/foo/libfoo_stubs.a

  $ dune exec ./exe/b.exe
  exe working

  $ rm -rf lib
  $ OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  exe working
