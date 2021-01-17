
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

  $ rm -rf .git
