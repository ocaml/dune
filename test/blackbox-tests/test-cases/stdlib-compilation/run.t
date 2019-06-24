  $ touch stdlib.opam
  $ dune build
  File "src/dune", line 1, characters 0-199:
  1 | (library
  2 |  (public_name stdlib)
  3 |  (stdlib
  4 |   (modules_before_stdlib CamlinternalFormatBasics)
  5 |   (exit_module std_exit)
  6 |   (internal_modules Camlinternal*))
  7 |  (preprocess (action (run cat %{input-file}))))
  Error: No rule found for src/.stdlib.objs/native/stdlib__.cmx
  File "src/dune", line 1, characters 0-199:
  1 | (library
  2 |  (public_name stdlib)
  3 |  (stdlib
  4 |   (modules_before_stdlib CamlinternalFormatBasics)
  5 |   (exit_module std_exit)
  6 |   (internal_modules Camlinternal*))
  7 |  (preprocess (action (run cat %{input-file}))))
  Error: No rule found for src/.stdlib.objs/native/stdlib__$ext_obj
  File "src/dune", line 1, characters 0-199:
  1 | (library
  2 |  (public_name stdlib)
  3 |  (stdlib
  4 |   (modules_before_stdlib CamlinternalFormatBasics)
  5 |   (exit_module std_exit)
  6 |   (internal_modules Camlinternal*))
  7 |  (preprocess (action (run cat %{input-file}))))
  Error: No rule found for src/.stdlib.objs/byte/stdlib__.cmo
  File "src/dune", line 1, characters 0-199:
  1 | (library
  2 |  (public_name stdlib)
  3 |  (stdlib
  4 |   (modules_before_stdlib CamlinternalFormatBasics)
  5 |   (exit_module std_exit)
  6 |   (internal_modules Camlinternal*))
  7 |  (preprocess (action (run cat %{input-file}))))
  Error: No rule found for src/.stdlib.objs/byte/stdlib__.cmi
  File "src/dune", line 1, characters 0-199:
  1 | (library
  2 |  (public_name stdlib)
  3 |  (stdlib
  4 |   (modules_before_stdlib CamlinternalFormatBasics)
  5 |   (exit_module std_exit)
  6 |   (internal_modules Camlinternal*))
  7 |  (preprocess (action (run cat %{input-file}))))
  Error: No rule found for src/.stdlib.objs/byte/stdlib__.cmt
  File "src/dune", line 1, characters 0-199:
  1 | (library
  2 |  (public_name stdlib)
  3 |  (stdlib
  4 |   (modules_before_stdlib CamlinternalFormatBasics)
  5 |   (exit_module std_exit)
  6 |   (internal_modules Camlinternal*))
  7 |  (preprocess (action (run cat %{input-file}))))
  Error: No rule found for src/stdlib__.ml-gen
        ocamlc src/.stdlib.objs/byte/camlinternalFormatBasics.{cmi,cmo,cmt} (exit 2)
  (cd _build/default && /Users/rgrinberg/.opam/4.07.1/bin/ocamlc.opt -w @a-4-29-40-41-42-44-45-48-58-59-60-66-40 -strict-sequence -strict-formats -short-paths -keep-locs -g -bin-annot -I src/.stdlib.objs/byte -no-alias-deps -opaque -open Stdlib__ -nopervasives -nostdlib -o src/.stdlib.objs/byte/camlinternalFormatBasics.cmo -c -impl src/camlinternalFormatBasics.pp.ml)
  File "command line", line 1:
  Error: Unbound module Stdlib__
  [1]
