Installing multiple directories into share_root currently crashes dune

Only one package, result is as expected
  $ dune build @install --only a
  $ ls _build/install/default/share
  readme_a.txt

The other package, result is as expected
  $ dune build @install --only b
  $ ls _build/install/default/share
  readme_b.txt

  $ dune clean

Both at the same time leads to a crash
  $ dune build @install 2>&1 | grep "must not crash"
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  [1]
  $ tree -d _build
  _build
  `-- default
  
  2 directories
