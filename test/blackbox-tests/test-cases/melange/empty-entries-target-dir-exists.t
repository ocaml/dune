Test (entries) field can be left empty

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (alias melange-dist)
  >  (target dist)
  >  (module_system commonjs))
  > EOF

Create the dist folder

  $ mkdir ./dist

  $ dune build @melange-dist
  $ ls _build/default/dist
  melange.js
