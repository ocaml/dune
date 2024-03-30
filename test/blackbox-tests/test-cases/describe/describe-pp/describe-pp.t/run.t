We can show the preprocessed output of a source code

  $ dune describe pp src/main.ml
  ;;Util.log "Hello, world!"

Re-running the command keeps showing output

  $ dune describe pp src/main.ml
  ;;Util.log "Hello, world!"

We can also show the original source if it is not preprocessed

  $ dune describe pp src/util.ml
  let log str = print_endline str

We also make sure that the dump file is not present

  $ dune_cmd exists profile.dump
  false

This also works for reason code

  $ dune describe pp src/main_re.re
  # 1 "src/main_re.pp.re.ml"
  # 1 "src/main_re.pp.re"
  Util.log ("Hello, world!")
