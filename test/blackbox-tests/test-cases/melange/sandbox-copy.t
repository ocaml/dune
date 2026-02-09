Melange rules should work with the copying sandbox

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (using melange 1.0)
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name lib)
  >  (modes melange)
  >  (modules lib))
  > (melange.emit
  >  (target output)
  >  (alias mel)
  >  (modules main)
  >  (libraries lib)
  >  (emit_stdlib false)
  >  (runtime_deps assets/data.txt))
  > EOF

  $ cat > lib.ml <<EOF
  > let message = "hello from lib"
  > EOF

  $ cat > main.ml <<EOF
  > let () = Js.log Lib.message
  > EOF

  $ mkdir -p assets
  $ echo "asset ok" > assets/data.txt

  $ rm -rf _build
  $ dune build @mel --sandbox copy --display quiet

Build artifacts are available in the melange target directory

  $ ls _build/default/output
  assets
  lib.js
  main.js
  $ cat _build/default/output/assets/data.txt
  asset ok
