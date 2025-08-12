Show target promotion in-source for `melange.emit`

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (using melange 1.0)
  > EOF

  $ mkdir -p app/assets
  $ cat > app/assets/file.txt <<EOF
  > hello from file
  > EOF

  $ cat > app/dune <<EOF
  > (include_subdirs unqualified)
  > (melange.emit
  >  (alias dist)
  >  (emit_stdlib false)
  >  (promote (until-clean))
  >  (target dist)
  >  (runtime_deps assets/file.txt))
  > EOF
  $ cat > app/x.ml <<EOF
  > let () = print_endline "hello"
  > EOF
  $ dune build @dist

If we're emitting back to source, `runtime_deps` will overwrite the original
files by default

  $ ls app
  assets
  dune
  x.js
  x.ml

  $ cat app/assets/file.txt
  hello from file

Customize the promotion dir:

  $ cat > app/dune <<EOF
  > (include_subdirs unqualified)
  > (melange.emit
  >  (alias dist)
  >  (emit_stdlib false)
  >  (promote (until-clean) (into dist))
  >  (target dist)
  >  (runtime_deps assets/file.txt))
  > EOF
  $ cat > app/x.ml <<EOF
  > let () = print_endline "hello"
  > EOF
  $ dune build @dist

  $ ls app/dist
  assets
  x.js
  $ cat app/dist/assets/file.txt
  hello from file

