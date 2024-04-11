Test a case of virtual libraries where libraries depend on the virtual library
transitively

  $ mkdir -p common compiler pal pal_js
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (using melange 0.1)
  > EOF
  $ cat > compiler/dune <<EOF
  > (library
  >  (name compiler)
  >  (modes melange)
  >  (libraries common))
  > EOF

  $ cat > common/dune <<EOF
  > (library
  >  (name common)
  >  (modes melange)
  >  (wrapped false)
  >  (libraries pal))
  > EOF
  $ cat > common/stringBuilder.ml <<EOF
  > include Pal.StringBuilder
  > EOF

  $ cat > pal/dune <<EOF
  > (library
  >  (name pal)
  >  (modes melange)
  >  (virtual_modules stringBuilder))
  > EOF
  $ cat > pal/stringBuilder.mli <<EOF
  > type t
  > EOF

  $ cat > pal_js/dune <<EOF
  > (library
  >  (name pal_js)
  >  (modes melange)
  >  (implements pal))
  > EOF
  $ cat > pal_js/stringBuilder.ml <<EOF
  > type t = string ref
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target js)
  >  (modules)
  >  (emit_stdlib false)
  >  (libraries compiler pal_js))
  > EOF

  $ dune build @melange
  File "_none_", line 1:
  Error: Pal__StringBuilder not found, it means either the module does not exist or it is a namespace
  [1]
