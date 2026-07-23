When the version is absent, we print the empty string

  $ make_dune_project_with_package 3.13 foo

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (action (echo foo is '%{version:foo}')))
  > EOF

  $ dune build @foo
  foo is ''

