Tests for enabled_if in install stanza. Only bar.x should be installed.

  $ dune build @install
  $ ls _build/install/default/bin
  bar.x
