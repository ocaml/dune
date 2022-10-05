Testing coq.of-ocaml stanza

  $ dune build hello.v
  fake coq-of-ocaml has run

  $ ls _build/default/
  hello.ml
  hello.v

  $ cat _build/default/hello.v
  coq-of-ocaml has run on hello.ml
