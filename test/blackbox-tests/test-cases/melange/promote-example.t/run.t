Test melange.emit promotion with runtime_deps and into

  $ dune build @dist

Targets are promoted to the source tree

  $ tree _build
  _build
  |-- default
  |   `-- test
  |       |-- dist
  |       |   `-- test
  |       |       |-- snapshots
  |       |       |   `-- 0.js
  |       |       `-- test.js
  |       |-- snapshots
  |       |   `-- 0.js
  |       `-- test.ml
  `-- log
  
  6 directories, 5 files


  $ tree test
