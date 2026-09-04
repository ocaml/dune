Test dependency on installed package

  $ mkdir a b prefix

  $ cat > a/dune-project <<EOF
  > (lang dune 3.7)
  > (package (name a))
  > (using experimental_building_ocaml_compiler_with_dune 0.1)
  > EOF
  $ cat > a/dune <<EOF
  > (library
  >  (name a)
  >  (public_name a)
  >  (stdlib
  >   (exit_module Std_exit)
  >   (modules_before_stdlib CamlinternalFormatBasics)
  >   (internal_modules Camlinternal*)))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ cat > a/a.ml <<EOF
  > module Foo = A__Foo
  > EOF

  $ touch a/camlinternalFoo.ml a/std_exit.ml

  $ dune build --root a

  $ dune install --root a --prefix $PWD/prefix --display short
  Installing $TESTCASE_ROOT/prefix/lib/a/META
  Installing $TESTCASE_ROOT/prefix/lib/a/a.a
  Installing $TESTCASE_ROOT/prefix/lib/a/a.cma
  Installing $TESTCASE_ROOT/prefix/lib/a/a.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/a.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/a.cmx
  Installing $TESTCASE_ROOT/prefix/lib/a/a.cmxa
  Installing $TESTCASE_ROOT/prefix/lib/a/a.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/a__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/a__Foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/a__Foo.cmx
  Installing $TESTCASE_ROOT/prefix/lib/a/camlinternalFoo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/camlinternalFoo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/camlinternalFoo.cmx
  Installing $TESTCASE_ROOT/prefix/lib/a/camlinternalFoo.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/a/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/std_exit.cmi
  Installing $TESTCASE_ROOT/prefix/lib/a/std_exit.cmt
  Installing $TESTCASE_ROOT/prefix/lib/a/std_exit.cmx
  Installing $TESTCASE_ROOT/prefix/lib/a/std_exit.ml
  Installing $TESTCASE_ROOT/prefix/lib/a/a.cmxs

  $ grep -o '(path [A-Z][A-Za-z_ ]*)' prefix/lib/a/dune-package
  (path A)
  (path CamlinternalFoo)
  (path A Foo)
  (path Std_exit)

  $ cat >b/dune-project <<EOF
  > (lang dune 3.7)
  > (package (name b))
  > EOF

  $ cat > b/dune <<EOF
  > (library
  >  (public_name b)
  >  (libraries a))
  > EOF

  $ cat > b/bar.ml <<EOF
  > let x = A.Foo.x
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root b @install

Unwrapped stdlib modules keep unqualified source paths:

  $ mkdir unwrapped
  $ cat >unwrapped/dune-project <<'EOF'
  > (lang dune 3.7)
  > (package (name unwrapped))
  > (using experimental_building_ocaml_compiler_with_dune 0.1)
  > EOF
  $ cat >unwrapped/dune <<'EOF'
  > (library
  >  (name unwrapped)
  >  (public_name unwrapped)
  >  (wrapped false)
  >  (stdlib))
  > EOF
  $ touch unwrapped/unwrapped.ml unwrapped/foo.ml
  $ dune build unwrapped.install --root=unwrapped
  $ grep -o '(path [A-Z][A-Za-z_ ]*)' \
  > unwrapped/_build/install/default/lib/unwrapped/dune-package
  (path Foo)
  (path Unwrapped)
