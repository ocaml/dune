Show target promotion in-source for `melange.emit`

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (using melange 1.0)
  > EOF

  $ mkdir -p app
  $ cat > app/dune <<EOF
  > (include_subdirs unqualified)
  > (melange.emit
  >  (alias dist)
  >  (emit_stdlib false)
  >  (promote (until-clean))
  >  (target dist))
  > EOF
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

  $ cat > app/dune <<EOF
  > (include_subdirs unqualified)
  > (melange.emit
  >  (alias dist)
  >  (emit_stdlib false)
  >  (promote (into .) (until-clean))
  >  (target dist))
  > EOF
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

  $ cat > app/dune <<EOF
  > (include_subdirs unqualified)
  > (melange.emit
  >  (alias dist)
  >  (emit_stdlib false)
  >  (promote (into ../toplevel-dist) (until-clean))
  >  (target dist))
  > EOF

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

  $ cat > app/dune <<EOF
  > (include_subdirs unqualified)
  > (melange.emit
  >  (alias dist)
  >  (emit_stdlib false)
  >  (promote (into ../../foo))
  >  (target dist))
  > EOF
  $ dune build @dist
  File "app/dune", line 5, characters 16-25:
  5 |  (promote (into ../../foo))
                      ^^^^^^^^^
  Error: path cannot escape the context root
  [1]

  $ dune clean

It's possible to recover the behavior of emitting in the dist folder with
`(promote (into ..))`

  $ cat > app/dune <<EOF
  > (include_subdirs unqualified)
  > (melange.emit
  >  (alias dist)
  >  (emit_stdlib false)
  >  (promote (into ./dist) (until-clean))
  >  (target dist))
  > EOF
  $ dune build @dist
  $ ls app/dist
  other
  x.js
  $ ls app/dist/other
  other.js

