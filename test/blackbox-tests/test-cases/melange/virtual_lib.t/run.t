Test virtual lib in an exe / melange environment

  $ dune build output/mel.js
  File "impl_melange/dune", line 1, characters 0-66:
  1 | (library
  2 |  (name impl_melange)
  3 |  (modes melange)
  4 |  (implements vlib))
  Error: No rule found for output/vlib/shared.js
  File "impl_melange/dune", line 1, characters 0-66:
  1 | (library
  2 |  (name impl_melange)
  3 |  (modes melange)
  4 |  (implements vlib))
  Error: No rule found for output/vlib/vlib_impl.js
  [1]
  $ output=_build/default/output/mel.js
  $ test -f "$output" && node "$output"
  [1]
