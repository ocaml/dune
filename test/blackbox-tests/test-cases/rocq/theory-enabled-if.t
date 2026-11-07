A disabled rocq.theory must not declare its rocqdoc directory targets nor
install its files.

  $ make_rocq_project 3.24 0.14
  $ cat > a.v <<EOF
  > Definition x := 1.
  > EOF

A theory disabled with (enabled_if false):

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name Foo)
  >  (enabled_if false))
  > EOF

  $ dune build
  $ dune build @install
  $ ls _build/default/*.vo
  ls: cannot access '_build/default/*.vo': No such file or directory
  [2]

The same theory, enabled or disabled through an environment variable:

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name Foo)
  >  (enabled_if %{env:FOO_ENABLED=false}))
  > EOF

  $ dune build
  $ ls _build/default/*.vo
  ls: cannot access '_build/default/*.vo': No such file or directory
  [2]

  $ FOO_ENABLED=true dune build
  $ ls _build/default/*.vo
  _build/default/a.vo

  $ FOO_ENABLED=true dune build Foo.html/
  $ ls _build/default/Foo.html
  Foo.a.html
  coqdoc.css
  index.html
  toc.html

A disabled theory attached to a package is not installed. As for a disabled
library, a package without any other stanza is then reported as empty:

  $ cat >> dune-project <<EOF
  > (package (name foo))
  > EOF
  $ cat > dune <<EOF
  > (rocq.theory
  >  (name Foo)
  >  (package foo)
  >  (enabled_if %{env:FOO_ENABLED=false}))
  > EOF

  $ dune build @install
  Error: The package foo does not have any user defined stanzas attached to it.
  If this is intentional, add (allow_empty) to the package definition in the
  dune-project file
  -> required by _build/default/foo.install
  -> required by alias install
  [1]

  $ FOO_ENABLED=true dune build @install
  $ grep 'a\.vo' _build/default/foo.install
    "_build/install/default/lib/coq/user-contrib/Foo/a.vo" {"coq/user-contrib/Foo/a.vo"}

With (allow_empty), the package installs nothing from the disabled theory:

  $ cat > dune-project <<EOF
  > (lang dune 3.24)
  > (using rocq 0.14)
  > (package (name foo) (allow_empty))
  > EOF

  $ dune build @install
  $ grep 'a\.vo' _build/default/foo.install
  [1]
