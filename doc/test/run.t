----------------------------------------------------------------------------------
Reveal all occurrences of non-latest (lang dune ...) in examples.

  $ touch dune
  $ dune build > /dev/null 2> /dev/null
  $ DUNE_LANG=$(cat dune-project)
  $ grep '(lang dune' ../*.rst | grep -v "$DUNE_LANG"
  ../formatting.rst:If using ``(lang dune 2.0)``, there is nothing to do, formatting will be set up
  ../formatting.rst:.. note:: This section applies only to projects with ``(lang dune 1.x)``.
  ../formatting.rst:In ``(lang dune 1.x)``, no formatting is done by default. This feature is
  ../formatting.rst:(lang dune 2.0)
