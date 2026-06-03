Show target promotion in-source for `melange.emit`

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (using melange 1.0)
  > EOF

  $ mkdir -p app
  $ write_melange_promote_app_dune "(until-clean)"
  $ cat > app/x.ml <<EOF
  > let () = print_endline "hello"
  > EOF
  $ dune build @dist

Targets get emitted in source

  $ ls app
  dune
  x.js
  x.ml

  $ mkdir app/other
  $ cat > app/other/other.ml <<EOF
  > let () = print_endline "other"
  > EOF
  $ dune build @dist
  $ ls app/other
  other.js
  other.ml

Dune clean gets rid of them

  $ dune clean
  $ ls app
  dune
  other
  x.ml
  $ ls app/other
  other.ml

`(into .)` is the same

  $ write_melange_promote_app_dune "(into .) (until-clean)"
  $ dune build @dist

Targets get emitted in source

  $ ls app
  dune
  other
  x.js
  x.ml

  $ ls app/other
  other.js
  other.ml

Dune clean gets rid of them

  $ dune clean
  $ ls app
  dune
  other
  x.ml
  $ ls app/other
  other.ml

emit into a different dest dir

  $ write_melange_promote_app_dune "(into ../toplevel-dist) (until-clean)"

  $ dune build @dist
  $ ls toplevel-dist
  other
  x.js
  $ ls toplevel-dist/other
  other.js

Cleaning also works

  $ dune clean
  $ ls toplevel-dist
  other
  $ ls toplevel-dist/other

Promote dir can't be outside the workspace

  $ write_melange_promote_app_dune "(into ../../foo)"
  $ dune build @dist
  File "app/dune", line 5, characters 16-25:
  5 |  (promote (into ../../foo))
                      ^^^^^^^^^
  Error: path cannot escape the context root
  [1]

  $ dune clean

It's possible to recover the behavior of emitting in the dist folder with
`(promote (into ..))`

  $ write_melange_promote_app_dune "(into ./dist) (until-clean)"
  $ dune build @dist
  $ ls app/dist
  other
  x.js
  $ ls app/dist/other
  other.js

