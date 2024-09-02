  $ dune build @check

  $ export DUNE_CONFIG__SKIP_LINE_BREAK=enabled
  $ dune ocaml merlin dump-config $PWD | grep -o '(SUFFIX.*)'
  (SUFFIX ".aml .amli") (SUFFIX ".baml .bamli"))
  (SUFFIX ".aml .amli") (SUFFIX ".baml .bamli"))
