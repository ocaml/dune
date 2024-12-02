----------------------------------------------------------------------------------
Reveal all occurrences of non-latest (lang dune ...) version in documentation.

When changing Dune version, you need to update the docs too to make this test pass.

Occasionally we do want to mention an older Dune version in documentation. This
is fine, but you then need to update the list of such exceptions below.

  $ DUNE_LANG=$(dune internal latest-lang-version)
  $ grep '(lang dune' ../*.rst | grep -v "$DUNE_LANG"
  ../coq.rst:  (lang dune 3.17)
  ../coq.rst:  (lang dune 3.17)
  ../foreign-code.rst:  (lang dune 3.17)
  ../hacking.rst:``(lang dune 2.7)`` in their ``dune`` project file to use it.
  ../hacking.rst:   (lang dune 3.17)
  ../instrumentation.rst:   (lang dune 3.17)
  ../instrumentation.rst:   (lang dune 3.17)
  ../melange.rst:  (lang dune 3.17)
  ../sites.rst:   (lang dune 3.17)
  ../sites.rst:  (lang dune 3.17)
  ../sites.rst:  (lang dune 3.17)
  ../tests.rst:   (lang dune 2.7)
