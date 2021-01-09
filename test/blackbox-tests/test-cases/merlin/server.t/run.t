  $ FILE=$PWD/main.ml
  $ dune ocaml-merlin  <<EOF
  > (4:File${#FILE}:$FILE)
  > EOF
  ((5:ERROR48:Project isn't built. (Try calling `dune build`.)))

  $ dune build @check 2>&1 | sed "s/(lang dune .*)/(lang dune <version>)/"
  Info: Creating file dune-project with this contents:
  | (lang dune <version>)

  $ dune ocaml-merlin <<EOF | sed -E "s/[[:digit:]]+:/\?:/g"
  > (4:File${#FILE}:$FILE)
  > EOF
  ((?:ERROR?:Project isn't built. (Try calling `dune build`.)))

  $ FILE=$PWD/lib3.ml
  $ dune ocaml-merlin <<EOF | sed -E "s/[[:digit:]]+:/\?:/g"
  > (4:File${#FILE}:$FILE)
  > EOF
  ((?:ERROR?:Project isn't built. (Try calling `dune build`.)))

