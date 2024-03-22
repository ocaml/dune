Test dependency on installed package

  $ mkdir xyz

  $ cat > xyz/dune-project <<EOF
  > (lang dune 3.8)
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
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (alias dist)
  >  (emit_stdlib false)
  >  (libraries aa_fe.melange))
  > EOF

  $ cat > bar.ml <<EOF
  > let x = Js.log Aa_fe_melange.Foo.x
  > EOF

  $ dune build @dist

  $ node _build/default/dist/bar.js
  foo

Move inner lib to a subfolder inside its dune-project

  $ cd xyz
  $ mkdir inner
  $ mv dune-project inner
  $ mv dune inner
  $ mv foo.ml inner
  $ cd -
  $TESTCASE_ROOT

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

Move back inner lib to main folder

  $ cd xyz/inner
  $ mv dune-project ..
  $ mv dune ..
  $ mv foo.ml ..
  $ cd -
  $TESTCASE_ROOT

  $ dune build @dist

  $ node _build/default/abc/dist/abc/bar.js
  foo
