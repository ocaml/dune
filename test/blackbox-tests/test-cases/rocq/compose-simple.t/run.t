Testing a simple composition of theories. We have two theories A and B and B
depends on A.

  $ dune build

We inspect the contents of the build directory.

  $ ls _build/install/default/lib/coq/user-contrib/A/a.vo
  _build/install/default/lib/coq/user-contrib/A/a.vo
  $ ls _build/default/A/a.vo
  _build/default/A/a.vo
  $ ls _build/install/default/lib/coq/user-contrib/B/b.vo
  _build/install/default/lib/coq/user-contrib/B/b.vo
  $ ls _build/default/B/b.vo
  _build/default/B/b.vo
