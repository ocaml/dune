Demonstrate that there should be no dependence on the dune lang version for
building packages:

  $ . ./helpers.sh

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build (run echo foo))
  > EOF

  $ makeProject() {
  > cat >dune-project <<EOF
  > (lang dune 3.20)
  > (expand_aliases_in_sandbox $1)
  > (package
  >  (name foo)
  >  (depends test))
  > EOF
  > build_pkg test
  > }

There should only be one execution here:

  $ makeProject true
  foo

  $ makeProject false
  foo
