Test that post dependencies are installed in the order in which they
appear in lockfiles.

  $ mkdir dune.lock

  $ cat > dune.lock/lock.dune << EOF
  > (lang package 0.1)
  > EOF

Create some simple packages with no dependencies:
  $ simple_package() {
  >   cat > dune.lock/$1.pkg << EOF
  > (version 0.1)
  > (install
  >  (run echo Installing package %{pkg-self:name}...))
  > EOF
  > }

  $ simple_package a
  $ simple_package b
  $ simple_package c
  $ simple_package d
  $ simple_package e

Create packages with post dependencies on simple dependencies:

  $ cat > dune.lock/post-dep-on-ba.pkg << EOF
  > (version 0.1)
  > (post_depends b a)
  > (install
  >  (run echo Installing package %{pkg-self:name}...))
  > EOF

  $ cat > dune.lock/post-dep-on-dc.pkg << EOF
  > (version 0.1)
  > (post_depends d c)
  > (install
  >  (run echo Installing package %{pkg-self:name}...))
  > EOF

  $ cat > dune.lock/post-dep-on-edc.pkg << EOF
  > (version 0.1)
  > (post_depends e d c)
  > (install
  >  (run echo Installing package %{pkg-self:name}...))
  > EOF

  $ cat > dune.lock/dep-on-everything.pkg << EOF
  > (version 0.1)
  > (depends post-dep-on-dc post-dep-on-edc post-dep-on-ba)
  > EOF

The order in which post dependencies (the single-letter name packages
here) are installed is the order in which they are depended upon by
each package among the dependencies of dep-on-everything.
  $ cat > dune-project << EOF
  > (lang dune 3.16)
  > (package (name x)
  >  (allow_empty)
  >  (depends dep-on-everything))
  > EOF

  $ dune build
  Installing package post-dep-on-dc...
  Installing package post-dep-on-edc...
  Installing package post-dep-on-ba...
  Installing package d...
  Installing package c...
  Installing package e...
  Installing package b...
  Installing package a...
