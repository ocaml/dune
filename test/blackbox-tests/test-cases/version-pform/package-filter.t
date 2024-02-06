Here we are referring to a package that is external with -p, but we are still
allowed to access it:

  $ make_project() {
  > cat >dune-project <<EOF
  > (lang dune 3.13)
  > (package (name a))
  > (package (name b))
  > (version $1)
  > EOF
  > }

  $ make_project 1.0.0

  $ mkdir a b
  $ cat >a/dune <<EOF
  > (library (public_name a))
  > EOF
  $ cat >b/dune <<EOF
  > (library (public_name b))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (deps (universe))
  >  (action (echo version a is '%{version:a}')))
  > EOF

  $ dune build @foo -p b
  version a is '1.0.0'

Not only that the above is inconsistent with how we handle external packages,
it will also give the wrong answer if a was installed.

  $ dune build @install -p a
  $ dune install a --prefix _install

The answer should be 1.0.0, bceause that's the version we installed

  $ make_project 2.0.0
  $ OCAMLPATH="$PWD/_install/lib:$OCAMLPATH" dune build @foo -p b
  version a is '2.0.0'
