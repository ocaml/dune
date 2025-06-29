When the version is absent, we print the empty string

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (package (name foo))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (action (echo foo is '%{version:foo}')))
  > EOF

  $ dune build @foo
  foo is ''

