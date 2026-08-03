Reports modules that are claimed by multiple stanzas.

  $ dune exec ./test.exe --debug-dep
  File "dune", line 1, characters 0-0:
  Error: Module "Lib" is used in several stanzas:
  - dune:1
  - dune:5
  To fix this error, you must specify explicit "modules" fields so that each
  module belongs to only one stanza. Stanzas without an explicit "modules"
  field use all modules in the directory by default. This applies to library,
  executable, executables, test, tests, and melange.emit stanzas.
  [1]

  $ dune build src/a.cma --debug-dep
  File "src/dune", line 1, characters 0-0:
  Error: Module "X" is used in several stanzas:
  - src/dune:1
  - src/dune:2
  To fix this error, you must specify explicit "modules" fields so that each
  module belongs to only one stanza. Stanzas without an explicit "modules"
  field use all modules in the directory by default. This applies to library,
  executable, executables, test, tests, and melange.emit stanzas.
  [1]
