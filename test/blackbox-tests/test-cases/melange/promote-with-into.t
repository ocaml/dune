Promotion with targets `(into ..)` a directory

  $ make_melange_project 3.8 0.1

  $ mkdir app
  $ mkdir app/foo
  $ write_melange_promote_app_dune "(into ../../foo)"
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

