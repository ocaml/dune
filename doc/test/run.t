----------------------------------------------------------------------------------
Reveal all occurrences of non-latest (lang dune ...) version in documentation.

When changing Dune version, you need to update the docs too to make this test pass.

Occasionally we do want to mention an older Dune version in documentation. This
is fine, but you then need to update the list of such exceptions below.

  $ DUNE_LANG=$(dune internal latest-lang-version)
  $ grep '(lang dune' ../*.rst | grep -v "$DUNE_LANG"
  ../hacking.rst:``(lang dune 2.7)`` in their ``dune`` project file to use it.
  ../tests.rst:   (lang dune 2.7)
