dune install should not write anything to _build/
  $ echo "(lang dune 2.8)" > dune-project
  $ dune install --prefix _install
  $ ls -aR _build
  .
  ..
  default
  log
  
  _build/default:
  .
  ..
  .dune
  
  _build/default/.dune:
  .
  ..
  .dune-keep
  configurator
  configurator.v2
