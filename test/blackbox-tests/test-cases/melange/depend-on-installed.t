Test dependency on installed package

  $ mkdir a b prefix app

  $ cat > a/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name a))
  > (using melange 0.1)
  > EOF
  $ cat > a/dune <<EOF
  > (library
  >  (modes melange)
  >  (public_name a))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ dune build --root a

  $ dune install --root a --prefix $PWD/prefix --display short
  Installing $TESTCASE_ROOT/prefix/lib/a/META
  Installing $TESTCASE_ROOT/prefix/lib/a/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a__Foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/a__Foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/melange/foo.ml

  $ cat >b/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name b))
  > (using melange 0.1)
  > EOF

  $ cat > b/dune <<EOF
  > (library
  >  (modes melange)
  >  (public_name b)
  >  (libraries a))
  > EOF

  $ cat > b/bar.ml <<EOF
  > let x = A.Foo.x
  > EOF

  $ cat > b/foo.ml <<EOF
  > let x = Bar.x
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root b @install
  $ find b/_build/default/.b.objs/melange -type f | sort
  b/_build/default/.b.objs/melange/b.cmi
  b/_build/default/.b.objs/melange/b.cmj
  b/_build/default/.b.objs/melange/b.cmt
  b/_build/default/.b.objs/melange/b__Bar.cmi
  b/_build/default/.b.objs/melange/b__Bar.cmj
  b/_build/default/.b.objs/melange/b__Bar.cmt
  b/_build/default/.b.objs/melange/b__Foo.cmi
  b/_build/default/.b.objs/melange/b__Foo.cmj
  b/_build/default/.b.objs/melange/b__Foo.cmt

  $ dune install --root b --prefix $PWD/prefix --display=short
  Installing $TESTCASE_ROOT/prefix/lib/b/META
  Installing $TESTCASE_ROOT/prefix/lib/b/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b.cmi
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b.cmj
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b.cmt
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b.ml
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b__Bar.cmi
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b__Bar.cmj
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b__Bar.cmt
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b__Foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/b__Foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/bar.ml
  Installing $TESTCASE_ROOT/prefix/lib/b/melange/foo.ml

  $ cat >app/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name app))
  > (using melange 0.1)
  > EOF

  $ cat > app/dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (alias melange-dist)
  >  (emit_stdlib false)
  >  (libraries b))
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root app @melange-dist
  $ find app/_build/default/dist -type f | sort
  app/_build/default/dist/node_modules/a/a.js
  app/_build/default/dist/node_modules/a/foo.js
  app/_build/default/dist/node_modules/b/b.js
  app/_build/default/dist/node_modules/b/bar.js
  app/_build/default/dist/node_modules/b/foo.js

An explicitly empty [melange.libraries] field remains empty after installation.

  $ mkdir ocaml_dep shared_empty_melange consumer prefix-empty

  $ cat > ocaml_dep/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name ocaml_dep))
  > EOF

  $ cat > ocaml_dep/dune <<EOF
  > (library
  >  (modes byte)
  >  (public_name ocaml_dep))
  > EOF

  $ cat > ocaml_dep/foo.ml <<EOF
  > let value = "ocaml"
  > EOF

  $ dune build --root ocaml_dep @install
  $ dune install --root ocaml_dep --prefix $PWD/prefix-empty > /dev/null

  $ cat > shared_empty_melange/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name shared_empty_melange))
  > (using melange 0.1)
  > EOF

  $ cat > shared_empty_melange/dune <<EOF
  > (library
  >  (modes byte melange)
  >  (public_name shared_empty_melange)
  >  (libraries ocaml_dep)
  >  (melange.libraries ))
  > EOF

  $ cat > shared_empty_melange/shared_empty_melange.ml <<EOF
  > let value = Ocaml_dep.Foo.value
  > EOF

  $ cat > shared_empty_melange/shared_empty_melange.melange.ml <<EOF
  > let value = "melange"
  > EOF

  $ OCAMLPATH=$PWD/prefix-empty/lib/:$OCAMLPATH dune build --root shared_empty_melange @install
  $ dune install --root shared_empty_melange --prefix $PWD/prefix-empty > /dev/null

  $ grep -E "requires|melange_requires" prefix-empty/lib/shared_empty_melange/dune-package
   (requires ocaml_dep)
   (melange_requires)

  $ cat > consumer/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name consumer))
  > (using melange 0.1)
  > EOF

  $ cat > consumer/dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (alias melange-dist)
  >  (emit_stdlib false)
  >  (libraries shared_empty_melange))
  > EOF

  $ cat > consumer/main.ml <<EOF
  > let () = Js.log Shared_empty_melange.value
  > EOF

  $ OCAMLPATH=$PWD/prefix-empty/lib/:$OCAMLPATH dune build --root consumer @melange-dist
  $ node consumer/_build/default/dist/main.js
  melange

A non-empty [melange.libraries] field remains independent from an explicitly
empty [libraries] field after installation.

  $ mkdir melange_dep shared_empty_ocaml consumer-melange-dep prefix-melange-dep

  $ cat > melange_dep/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name melange_dep))
  > (using melange 0.1)
  > EOF
  $ cat > melange_dep/dune <<EOF
  > (library
  >  (modes melange)
  >  (public_name melange_dep))
  > EOF
  $ cat > melange_dep/foo.ml <<EOF
  > let value = "melange dep"
  > EOF

  $ dune build --root melange_dep @install
  $ dune install --root melange_dep --prefix $PWD/prefix-melange-dep > /dev/null

  $ cat > shared_empty_ocaml/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name shared_empty_ocaml))
  > (using melange 0.1)
  > EOF
  $ cat > shared_empty_ocaml/dune <<EOF
  > (library
  >  (modes byte melange)
  >  (public_name shared_empty_ocaml)
  >  (libraries )
  >  (melange.libraries melange_dep))
  > EOF
  $ cat > shared_empty_ocaml/shared_empty_ocaml.ml <<EOF
  > let value = "ocaml"
  > EOF
  $ cat > shared_empty_ocaml/shared_empty_ocaml.melange.ml <<EOF
  > let value = Melange_dep.Foo.value
  > EOF

  $ OCAMLPATH=$PWD/prefix-melange-dep/lib/:$OCAMLPATH dune build --root shared_empty_ocaml @install
  $ dune install --root shared_empty_ocaml --prefix $PWD/prefix-melange-dep > /dev/null
  $ grep -E "requires|melange_requires" prefix-melange-dep/lib/shared_empty_ocaml/dune-package
   (melange_requires melange_dep)

  $ cat > consumer-melange-dep/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name consumer_melange_dep))
  > (using melange 0.1)
  > EOF
  $ cat > consumer-melange-dep/dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (alias melange-dist)
  >  (emit_stdlib false)
  >  (libraries shared_empty_ocaml))
  > EOF
  $ cat > consumer-melange-dep/main.ml <<EOF
  > let () = Js.log Shared_empty_ocaml.value
  > EOF

  $ OCAMLPATH=$PWD/prefix-melange-dep/lib/:$OCAMLPATH dune build --root consumer-melange-dep @melange-dist --display=quiet
  $ node consumer-melange-dep/_build/default/dist/main.js
  melange dep
