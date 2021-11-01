Allow directories to be installable

  $ cat >dune-project <<EOF
  > (lang dune 3.5)
  > (package (name foo))
  > (using directory-targets 0.1)
  > EOF

  $ cat >dune <<EOF
  > (install
  >  (dirs rules/bar)
  >  (section share))
  > EOF

  $ mkdir rules
  $ cat >rules/dune <<EOF
  > (rule
  >  (target (dir bar))
  >  (deps (sandbox always))
  >  (action (bash "mkdir -p %{target}/baz && touch %{target}/{x,y,z} && touch %{target}/baz/{a,b}")))
  > EOF

  $ dune build foo.install
  $ cat _build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
  ]
  share: [
    "_build/install/default/share/foo/bar/baz/a" {"bar/baz/a"}
    "_build/install/default/share/foo/bar/baz/b" {"bar/baz/b"}
    "_build/install/default/share/foo/bar/x" {"bar/x"}
    "_build/install/default/share/foo/bar/y" {"bar/y"}
    "_build/install/default/share/foo/bar/z" {"bar/z"}
  ]
