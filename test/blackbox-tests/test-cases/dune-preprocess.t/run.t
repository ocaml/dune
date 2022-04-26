This test check that we can show the preprocessed output of a source code

  $ dune preprocess src/main.ml
  ;;Util.log "Hello, world!"

We can also show the original source if it is not preprocessed

  $ dune preprocess src/util.ml
  let log str = print_endline str
