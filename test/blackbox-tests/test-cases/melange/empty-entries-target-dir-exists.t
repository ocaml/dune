Test (entries) field can be left empty

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ mkdir ./dist
  $ cat > dist/dune <<EOF
  > (melange.emit
  >  (alias melange-dist)
  >  (module_system commonjs))
  > EOF

  $ dune build @melange-dist --display=short
          melc dist/.dist.mobjs/melange/melange.{cmi,cmj,cmt}
          melc dist/dist/melange.js

  $ mkdir ./dist

  $ dune build @melange-dist
  $ ls _build/default/dist
