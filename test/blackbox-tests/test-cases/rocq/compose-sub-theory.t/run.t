Testing dependencies on subtheories. We have two theories A and B, but A is
defined as Foo.A. This changes the install layout of A.

  $ dune build

Inspecting the build and install directory
  $ ls _build/install/default/lib/coq/user-contrib/Foo/A/a.vo
  _build/install/default/lib/coq/user-contrib/Foo/A/a.vo
  $ ls _build/default/A/a.vo
  _build/default/A/a.vo
  $ ls _build/install/default/lib/coq/user-contrib/B/b.vo
  _build/install/default/lib/coq/user-contrib/B/b.vo
  $ ls _build/default/B/b.vo
  _build/default/B/b.vo
