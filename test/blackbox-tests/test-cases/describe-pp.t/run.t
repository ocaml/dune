We can show the preprocessed output of a source code

  $ dune describe pp src/main.ml
  ;;Util.log "Hello, world!"

We can also show the original source if it is not preprocessed

  $ dune describe pp src/util.ml
  let log str = print_endline str
