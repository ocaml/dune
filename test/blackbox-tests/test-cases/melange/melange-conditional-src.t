Show Melange-specific sources are conditionally compiled by Dune

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package  (name pkg) (allow_empty))
  > (using melange 1.0)
  > EOF

  $ mkdir a

  $ cat > a/dune <<EOF
  > (library
  >  (name a)
  >  (package pkg)
  >  (melange.compile_flags -w +a-70 -warn-error -102)
  >  (modes melange byte))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "ocaml"
  > EOF
  $ cat > a/foo.melange.ml <<EOF
  > let x = "melange"
  > let compare a b = compare a b
  > EOF

  $ dune build a/.melange_src/foo.ml
  $ cat _build/default/a/.melange_src/foo.ml
  # 1 "a/foo.melange.ml"
  let x = "melange"
  let compare a b = compare a b

  $ dune build _build/default/a/.a.objs/melange/a__Foo.cmi
  File "a/foo.melange.ml", line 2, characters 18-29:
  Warning 102 [polymorphic-comparison-introduced]: Polymorphic comparison introduced (maybe unsafe)

