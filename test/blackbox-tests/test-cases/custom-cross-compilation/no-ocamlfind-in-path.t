Dune shows an error message when ocamlfind isn't in PATH and OCAMLFIND_CONF
isn't set

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (package (name repro))
  > EOF
  $ cat > dune <<EOF
  > (executable
  >  (name gen)
  >  (modules gen)
  >  (enabled_if
  >   (= %{context_name} "default"))
  >  (libraries libdep))
  > (rule
  >  (with-stdout-to
  >   gen.ml
  >   (echo "let () = Format.printf \"let x = 1\"")))
  > (library
  >  (name repro)
  >  (public_name repro)
  >  (modules))
  > EOF

  $ mkdir ocaml-bin
  $ cat > ocaml-bin/ocamlc <<EOF
  > #!/usr/bin/env sh
  > export PATH="\$ORIG_PATH"
  > exec ocamlc "\$@"
  > EOF
  $ chmod +x ocaml-bin/ocamlc

  $ DUNE_PATH=$(dirname `which dune`)
  $ SH_PATH=$(dirname `which sh`)
  $ env ORIG_PATH="$PATH" PATH="$SH_PATH:$DUNE_PATH:$PWD/ocaml-bin" dune build @install -x foo
  Error: Could not find `ocamlfind' in PATH or an environment variable
  `OCAMLFIND_CONF' while cross-compiling with toolchain `foo'
  Hint:
  - `opam install ocamlfind' and/or:
  - Point `OCAMLFIND_CONF' to the findlib configuration that defines this
    toolchain
  [1]

