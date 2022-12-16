Test dependency on installed package

  $ mkdir a b prefix

  $ cat > a/dune-project <<EOF
  > (lang dune 3.6)
  > (package (name aa_fe))
  > (using melange 0.1)
  > EOF
  $ cat > a/dune <<EOF
  > (library
  >  (modes melange)
  >  (name aa_fe_melange)
  >  (public_name aa_fe.melange))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ dune build --root a
  Entering directory 'a'
  Leaving directory 'a'

  $ dune install --root a --prefix $PWD/prefix
  Installing $TESTCASE_ROOT/prefix/lib/aa_fe/META
  Installing $TESTCASE_ROOT/prefix/lib/aa_fe/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/aa_fe/melange/aa_fe_melange.ml
  Installing $TESTCASE_ROOT/prefix/lib/aa_fe/melange/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/aa_fe/melange/melange/aa_fe_melange.cmi
  Installing $TESTCASE_ROOT/prefix/lib/aa_fe/melange/melange/aa_fe_melange.cmj
  Installing $TESTCASE_ROOT/prefix/lib/aa_fe/melange/melange/aa_fe_melange.cmt
  Installing $TESTCASE_ROOT/prefix/lib/aa_fe/melange/melange/aa_fe_melange__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/aa_fe/melange/melange/aa_fe_melange__Foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/aa_fe/melange/melange/aa_fe_melange__Foo.cmt

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

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build @dist --display=short
          melc a/.aa_fe_melange.objs/melange/aa_fe_melange.{cmi,cmj,cmt}
      ocamldep a/.aa_fe_melange.objs/foo.ml.d
          melc dist/a/aa_fe_melange.js
          melc a/.aa_fe_melange.objs/melange/aa_fe_melange__Foo.{cmi,cmj,cmt}
          melc dist/a/foo.js
          melc .dist.mobjs/melange/melange__Bar.{cmi,cmj,cmt}
          melc dist/bar.js

  $ node _build/default/dist/bar.js
  node:internal/modules/cjs/loader:936
    throw err;
    ^
  
  Error: Cannot find module 'aa_fe/a/foo.js'
  Require stack:
  - $TESTCASE_ROOT/_build/default/dist/bar.js
      at Function.Module._resolveFilename (node:internal/modules/cjs/loader:933:15)
      at Function.Module._load (node:internal/modules/cjs/loader:778:27)
      at Module.require (node:internal/modules/cjs/loader:999:19)
      at require (node:internal/modules/cjs/helpers:102:18)
      at Object.<anonymous> ($TESTCASE_ROOT/_build/default/dist/bar.js:4:26)
      at Module._compile (node:internal/modules/cjs/loader:1099:14)
      at Object.Module._extensions..js (node:internal/modules/cjs/loader:1153:10)
      at Module.load (node:internal/modules/cjs/loader:975:32)
      at Function.Module._load (node:internal/modules/cjs/loader:822:12)
      at Function.executeUserEntryPoint [as runMain] (node:internal/modules/run_main:77:12) {
    code: 'MODULE_NOT_FOUND',
    requireStack: [
      '$TESTCASE_ROOT/_build/default/dist/bar.js'
    ]
  }
  
  Node.js v17.8.0
  [1]
