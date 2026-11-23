The middle library opens the dotted path Prelude.Color. Neither its source nor
the consumer names Prelude, but the consumer still needs Prelude's interfaces
to resolve the constructors in the sandbox.

  $ make_cross_lib_open_fixture color.ml t '-open Prelude.Color'
  $ dune build --sandbox=copy consumer/consumer.exe
