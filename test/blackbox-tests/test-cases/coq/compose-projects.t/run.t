Testing composition of theories across a dune workspace
  $ dune build B
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hello
       : Set

Inspecting the build directory
  $ ls _build/default/A/a.vo
  _build/default/A/a.vo
  $ ls _build/default/B/b.vo
  _build/default/B/b.vo
