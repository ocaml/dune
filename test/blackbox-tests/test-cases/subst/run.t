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

Project with only package stanza
--------------------------------
Repro for #2910.

  $ rm -f foo.opam

  $ cat > dune-project <<EOF
  > (lang dune 1.0)
  > (name foo)
  > (package (name foo))
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
  Error: No <package>.opam files found.
  [1]

  $ cat file.ml
  let name = "%%NAME%%"
  let authors = "%%PKG_AUTHORS%%"
  let version = "%%VERSION%%"

  $ cat dune-project
  (lang dune 1.0)
  (name foo)
  (package (name foo))

  $ rm -rf .git
