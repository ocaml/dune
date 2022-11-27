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
  > (install
  >  (dirs (rules/another-dir as renamed) (rules/nested as some/nesting/here))
  >  (section lib))
  > EOF

  $ mkdir rules
  $ cat >rules/dune <<EOF
  > (rule
  >  (target (dir bar))
  >  (deps (sandbox always))
  >  (action (bash "mkdir -p %{target}/baz && touch %{target}/{x,y,z} && touch %{target}/baz/{a,b}")))
  > (rule
  >  (target (dir another-dir))
  >  (deps (sandbox always))
  >  (action (chdir %{target} (run touch x))))
  > (rule
  >  (target (dir nested))
  >  (deps (sandbox always))
  >  (action (chdir %{target} (run touch x))))
  > EOF

  $ dune build foo.install
  $ cat _build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
    "_build/install/default/lib/foo/renamed/x" {"renamed/x"}
    "_build/install/default/lib/foo/some/nesting/here/x" {"some/nesting/here/x"}
  ]
  share: [
    "_build/install/default/share/foo/bar/baz/a" {"bar/baz/a"}
    "_build/install/default/share/foo/bar/baz/b" {"bar/baz/b"}
    "_build/install/default/share/foo/bar/x" {"bar/x"}
    "_build/install/default/share/foo/bar/y" {"bar/y"}
    "_build/install/default/share/foo/bar/z" {"bar/z"}
  ]

  $ mkdir ./installation
  $ dune install --prefix ./installation
  Installing installation/lib/foo/META
  Installing installation/lib/foo/dune-package
  Installing installation/lib/foo/renamed/x
  Installing installation/lib/foo/some/nesting/here/x
  Installing installation/share/foo/bar/baz/a
  Installing installation/share/foo/bar/baz/b
  Installing installation/share/foo/bar/x
  Installing installation/share/foo/bar/y
  Installing installation/share/foo/bar/z
  $ ls ./installation/lib/foo
  META
  dune-package
  renamed
  some
  $ ls ./installation/lib/foo/some/nesting/here
  x
