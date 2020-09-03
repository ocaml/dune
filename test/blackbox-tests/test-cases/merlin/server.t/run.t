  $ FILE=$PWD/main.ml
  $ dune ocaml-merlin  <<EOF
  > (4:File${#FILE}:$FILE)
  > EOF
  ((5:ERROR82:No configuration file found. Try calling `dune build` to generate `.merlin` files.))

  $ dune build @check 2>&1 | sed "s/(lang dune .*)/(lang dune <version>)/"
  Info: Creating file dune-project with this contents:
  | (lang dune <version>)
  $ dune ocaml-merlin  <<EOF
  > (4:File${#FILE}:$FILE)
  > EOF
  ((5:ERROR82:No configuration file found. Try calling `dune build` to generate `.merlin` files.))
