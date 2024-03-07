Testing dependencies on subtheories. We have two theories A and B, but A is
defined as Foo.A. This changes the install layout of A.

  $ dune build
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))

Inspecting the build and install directory
  $ ls _build/install/default/lib/coq/user-contrib/Foo/A/a.vo
  _build/install/default/lib/coq/user-contrib/Foo/A/a.vo
  $ ls _build/default/A/a.vo
  _build/default/A/a.vo
  $ ls _build/install/default/lib/coq/user-contrib/B/b.vo
  _build/install/default/lib/coq/user-contrib/B/b.vo
  $ ls _build/default/B/b.vo
  _build/default/B/b.vo
