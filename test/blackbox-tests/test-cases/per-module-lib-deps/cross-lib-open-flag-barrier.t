Opening Prelude in the middle library's compiler flags makes its color type
visible without naming Prelude in the source. The consumer must still track
prelude.cmi to resolve the constructors in the sandbox.

  $ make_cross_lib_open_fixture prelude.ml color '-open Prelude'
  $ dune build --sandbox=copy consumer/consumer.exe
