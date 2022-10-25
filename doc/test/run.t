----------------------------------------------------------------------------------
Reveal all occurrences of non-latest (lang dune ...) version in documentation.

When changing Dune version, you need to update the docs too to make this test pass.

Occasionally we do want to mention an older Dune version in documentation. This
is fine, but you then need to update the list of such exceptions below.

  $ DUNE_LANG=$(dune internal latest-lang-version)
  $ grep '(lang dune' ../*.rst | grep -v "$DUNE_LANG"
  ../dune-files.rst:    (lang dune 3.2)
  ../formatting.rst:If using ``(lang dune 2.0)``, there is nothing to setup in Dune, as formatting will
  ../formatting.rst:.. note:: This section applies only to projects with ``(lang dune 1.x)``.
  ../formatting.rst:In ``(lang dune 1.x)``, there is no default formatting. This feature is
  ../formatting.rst:(lang dune 2.0)
  ../hacking.rst:``(lang dune 2.7)`` in their ``dune`` project file to use it.
  ../tests.rst:   (lang dune 2.7)
