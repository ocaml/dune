Promotion with targets `(into ..)` a directory

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ mkdir app
  $ mkdir app/foo
  $ cat > app/dune <<EOF
  > (include_subdirs unqualified)
  > (melange.emit
  >  (alias dist)
  >  (emit_stdlib false)
  >  (promote (into ../../foo))
  >  (target dist))
  > EOF
  $ cat > app/x.ml <<EOF
  > let () = print_endline "hello"
  > EOF

  $ dune build @dist
  $ ls app/foo
  x.js

This hack breaks down as soon as you have some subdirs (since `(into <dir>)`
is relative to the artifact, not the dune file)

  $ mkdir app/other
  $ cat > app/other/other.ml <<EOF
  > let () = print_endline "other"
  > EOF
  $ dune build @dist

