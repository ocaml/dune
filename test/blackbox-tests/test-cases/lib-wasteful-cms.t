Testsuite to show wasteful rules being generated for some cm* files.

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name a)
  >  (modules a)
  >  (libraries foo))
  > EOF

  $ cat > a.ml <<EOF
  > let () =
  >   Foo.Z.f ()
  > EOF

  $ cat > b.ml <<EOF
  > let () =
  >   Foo.f ()
  > EOF

  $ mkdir foo

  $ cat > foo/dune <<EOF
  > (library
  >  (name foo))
  > EOF

  $ cat > foo/z.ml <<EOF
  > let f () =
  >   print_endline "12"
  > EOF

  $ cat > foo/y.ml <<EOF
  > let g () =
  >   print_endline "22"
  > EOF

Rules for foo__Y are created and the targets generated even if it is never used
  $ dune build --display short @all 2>&1 | dune_cmd sanitize | grep foo__Y
        ocamlc foo/.foo.objs/byte/foo__Y.{cmi,cmo,cmt}
      ocamlopt foo/.foo.objs/native/foo__Y.{cmx,o}

  $ (cd _build/default && ./a.exe)
  12

