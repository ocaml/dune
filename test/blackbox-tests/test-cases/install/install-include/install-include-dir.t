Example of including a file in the dirs field of the install stanza

  $ cat >dune-project <<EOF
  > (lang dune 3.5)
  > (package (name hello))
  > (using directory-targets 0.1)
  > EOF

  $ cat >dune <<EOF
  > (install
  >  (dirs (include baz.sexp))
  >  (section share))
  > EOF

  $ mkdir -p foo

  $ cat >foo/dune <<EOF
  > (rule
  >  (target (dir bar))
  >  (action (bash "mkdir %{target} && touch %{target}/a")))
  > EOF

  $ cat >baz.sexp <<EOF
  > (foo/bar)
  > EOF

  $ dune build @install

  $ cat _build/default/hello.install
  lib: [
    "_build/install/default/lib/hello/META"
    "_build/install/default/lib/hello/dune-package"
  ]
  share: [
    "_build/install/default/share/hello/bar/a" {"bar/a"}
  ]
