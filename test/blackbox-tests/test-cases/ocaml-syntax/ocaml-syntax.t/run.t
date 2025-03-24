  $ dune runtest --force
  ocaml syntax

Check that (include ...) works when generated using OCaml syntax.

  $ mkdir -p foo && cd foo
  $ cat >dune-project <<EOF
  > (lang dune 2.5)
  > EOF
  $ cat >dune <<EOF
  > (* -*- tuareg -*- *)
  > let () = Jbuild_plugin.V1.send {|(include dune.inc)|}
  > EOF
  $ cat >dune.inc <<EOF
  > (rule (with-stdout-to foo.txt (echo Hola!)))
  > EOF
  $ dune build --root . ./foo.txt
  $ cat _build/default/foo.txt
  Hola!
