Archive creation with unwrapped library, no stubs.
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
  >  (modules ()))
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
  $ find _build/install/default/lib/foo -name '*.a' 2>/dev/null | sort

  $ dune exec ./exe/b.exe
  exe working

  $ rm -rf lib
  $ OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  exe working
