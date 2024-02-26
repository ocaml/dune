Override a source when multiple projects in a workspace set it.

  $ . ../helpers.sh

Here we demonstrate that projects override their sub projects:

  $ mkdir a && cd a

  $ mkrepo
  $ add_mock_repo_if_needed

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "file://$PWD/_bar")
  >  (package (name bar)))
  > (package
  >  (name main)
  >  (depends bar))
  > EOF

  $ mkdir sub
  $ cat >sub/dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "file://$PWD/_bar_overriden")
  >  (package (name bar)))
  > EOF

  $ mkdir _bar
  $ cat >_bar/dune-project <<EOF
  > (lang dune 3.13)
  > (package (name bar))
  > EOF
  $ dune pkg lock
  Solution for dune.lock:
  - bar.dev

  $ print_source "bar"
  (source (fetch (url file://PWD/_bar))) (dev) 

  $ cd ..

However, when two projects are at the same level, dune is unable to correctly
select a priority:

  $ mkdir b && cd b

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkdir prj1
  $ cat >prj1/dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "file://$PWD/_bar1")
  >  (package (name bar)))
  > EOF

  $ mkdir prj2
  $ cat >prj2/dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "file://$PWD/_bar2")
  >  (package (name bar)))
  > EOF

  $ dune pkg lock
  File "prj1/dune-project", line 4, characters 1-21:
  4 |  (package (name bar)))
       ^^^^^^^^^^^^^^^^^^^^
  Error: package "bar" is defined in more than one source
  it is also defined in prj1/dune-project:4
  [1]
