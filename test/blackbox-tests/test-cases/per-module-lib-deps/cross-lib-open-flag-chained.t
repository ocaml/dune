The middle library opens Prelude and then Color. The second open resolves
through the first: Color is Prelude.Color, not a top-level module. The
consumer must still track Prelude's interfaces in the sandbox.

  $ make_cross_lib_open_fixture color.ml t '-open Prelude -open Color'
  $ dune build --sandbox=copy consumer/consumer.exe
