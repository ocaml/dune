Testing composition of theories across a dune workspace
  $ dune build B
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  Hello
       : Set

Inspecting the build directory
  $ ls _build/default/A/a.vo
  _build/default/A/a.vo
  $ ls _build/default/B/b.vo
  _build/default/B/b.vo
