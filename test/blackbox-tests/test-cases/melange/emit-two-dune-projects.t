Test dependency on installed package

  $ mkdir xyz

  $ cat > xyz/dune-project <<EOF
  > (lang dune 3.6)
  > (package (name aa_fe))
  > (using melange 0.1)
  > EOF
  $ cat > xyz/dune <<EOF
  > (library
  >  (modes melange)
  >  (name aa_fe_melange)
  >  (public_name aa_fe.melange))
  > EOF

  $ cat > xyz/foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.6)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (alias dist)
  >  (libraries aa_fe.melange)
  >  (module_system commonjs))
  > EOF

  $ cat > bar.ml <<EOF
  > let x = Js.log Aa_fe_melange.Foo.x
  > EOF

  $ dune build @dist

  $ node _build/default/dist/bar.js
  foo

Move everything except the workspace to a subfolder

  $ mkdir abc
  $ mv dune-project abc
  $ mv dune abc
  $ mv bar.ml abc

  $ dune build @dist

  $ node _build/default/abc/dist/abc/bar.js
  foo

