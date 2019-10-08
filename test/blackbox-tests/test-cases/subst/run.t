  $ cat > dune-project <<EOF
  > (lang dune 1.0)
  > (name foo)
  > EOF
  $ echo 'authors: [ "John Doe <john@doe.com>" ]' > foo.opam
  $ sed 's/%_%/%%/g' file.ml.in > file.ml
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
