Print a version while we're in the same project

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name foo))
  > (package
  >  (name bar))
  > (version 1.0.0)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (action (echo foo is '%{version:foo}' bar is '%{version:bar}')))
  > EOF

  $ dune build @foo
  foo is '1.0.0' bar is '1.0.0'

