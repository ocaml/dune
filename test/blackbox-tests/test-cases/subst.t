Project with opam files
-----------------------

  $ cat > dune-project <<EOF
  > (lang dune 1.0)
  > (name foo)
  > EOF

  $ X=%%; cat > file.ml <<EOF
  > let name = "${X}NAME${X}"
  > let authors = "${X}PKG_AUTHORS${X}"
  > let version = "${X}VERSION${X}"
  > EOF

  $ echo 'authors: [ "John Doe <john@doe.com>" ]' > foo.opam

  $ git init --quiet
  $ git add .
  $ git commit -am _ --quiet
  $ git tag -a 1.0 -m 1.0

  $ dune subst

  $ cat file.ml
  let name = "foo"
  let authors = "John Doe <john@doe.com>"
  let version = "1.0"

  $ cat dune-project
  (lang dune 1.0)
  (name foo)
  (version 1.0)

  $ rm -rf .git

Read from dune-project package stanza
-------------------------------------
And without an opam file preset.

  $ rm -f foo.opam

  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > (name foo)
  > (package (name foo) (authors "John Doe <john@doe.com>"))
  > EOF

  $ X=%%; cat > file.ml <<EOF
  > let name = "${X}NAME${X}"
  > let authors = "${X}PKG_AUTHORS${X}"
  > let version = "${X}VERSION${X}"
  > EOF

  $ git init --quiet
  $ git add .
  $ git commit -am _ --quiet
  $ git tag -a 1.0 -m 1.0

  $ dune subst

  $ cat file.ml
  let name = "foo"
  let authors = "John Doe <john@doe.com>"
  let version = "1.0"

  $ cat dune-project
  (lang dune 2.0)
  (name foo)
  (version 1.0)
  (package (name foo) (authors "John Doe <john@doe.com>"))

  $ rm -rf .git

Test subst and files with unicode (#3879)
-----------------------------------------

  $ rm -f foo.opam

  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > (name foo)
  > (package (name foo) (authors "John Doe <john@doe.com>"))
  > EOF

  $ X=%%; cat > α-term.ml <<EOF
  > let name = "${X}NAME${X}"
  > let authors = "${X}PKG_AUTHORS${X}"
  > let version = "${X}VERSION${X}"
  > EOF

  $ git init --quiet
  $ git add .
  $ git commit -am _ --quiet
  $ git tag -a 1.0 -m 1.0

  $ dune subst

  $ cat α-term.ml
  let name = "foo"
  let authors = "John Doe <john@doe.com>"
  let version = "1.0"

  $ cat dune-project
  (lang dune 2.0)
  (name foo)
  (version 1.0)
  (package (name foo) (authors "John Doe <john@doe.com>"))

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (name foo)
  > (version 1.0)
  > (package (name foo) (authors "John Doe <john@doe.com>"))
  > (subst disabled)
  > EOF

  $ dune subst
  File "dune-project", line 5, characters 7-15:
  5 | (subst disabled)
             ^^^^^^^^
  Error: 'dune subst' has been disabled in this project. Any use of it is
  forbidden.
  Hint: If you wish to re-enable it, change to (subst enabled) in the
  dune-project file.
  [1]

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (name foo)
  > (version 1.0)
  > (package (name foo) (authors "John Doe <john@doe.com>"))
  > (subst enabled)
  > EOF

  $ dune subst

  $ rm -rf .git
