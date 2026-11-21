Use interface inference to exercise anonymous actions. Changing shared-cache
policy must not cause an unchanged module to be type-checked again. A matching
interface makes the inference command succeed and its action cacheable.

  $ make_dune_project 3.26
  $ echo '(executable (name main) (modes byte))' > dune
  $ echo 'let answer = 42' > main.ml
  $ echo 'val answer : int' > main.mli
  $ inferences() {
  >   dune trace cat | jq -cs '
  >     [ .[]
  >       | select(.cat == "process" and .name == "start")
  >       | select(any(.args.process_args[]?; . == "-i"))
  >       | .args.process_args
  >       | map(select(. == "-i" or endswith("main.ml")))
  >       | map(split("/") | last)
  >     ]
  >   '
  > }
  $ DUNE_CACHE=disabled dune ocaml inferred-mli main.ml > /dev/null
  $ inferences
  [["-i","main.ml"]]
  $ DUNE_CACHE=disabled dune ocaml inferred-mli main.ml > /dev/null
  $ inferences
  []

Enabling the shared cache currently repeats the same inference action.

  $ DUNE_CACHE=enabled dune ocaml inferred-mli main.ml > /dev/null
  $ inferences
  [["-i","main.ml"]]
  $ DUNE_CACHE=enabled dune ocaml inferred-mli main.ml > /dev/null
  $ inferences
  []
  $ DUNE_CACHE=disabled dune ocaml inferred-mli main.ml > /dev/null
  $ inferences
  []
