Bigarray library is included in the Standard libraries since OCaml 5.00.0.
And now dune skip it by #5526 PR when included in libraries doing like "(libraries bigarray)".

  $ dummy="_build/default/c/c.dummy.ml"
  $ found="$(ocamlfind query bigarray -p-format)_found"
  $ cat > done.sh <<EOF
  > #!/usr/bin/env bash
  > if [[ "$found" != "bigarray_found" && -e $dummy ]]; then
  >   echo "Success skipping bigarray"
  > elif [[ "$found" != "bigarray_found" && ! -e $dummy ]]; then
  >   echo "Fail skipping bigarray"
  > else
  >   echo "Success skipping bigarray"
  > fi
  > EOF
  $ dune build @install
  $ chmod +x done.sh
  $ ./done.sh
  Success skipping bigarray
