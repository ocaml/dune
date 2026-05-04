A workspace declaring a cross-compile context via `(toolchain ...)`
that no rule actually uses must not require `ocamlfind` in PATH at
workspace setup. samoht's exact scenario from #10399.

  $ unset OCAMLFIND_TOOLCHAIN
  $ unset OCAMLFIND_CONF

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > EOF

The toolchain name [solo5] mirrors samoht's #10399 repro; any name
works, the test does not exercise solo5-specific tooling.

  $ cat > dune-workspace <<EOF
  > (lang dune 3.7)
  > (context (default))
  > (context (default (name solo5) (host default) (toolchain solo5)))
  > EOF

Wrappers for the OCaml toolchain tools dune resolves as siblings of
[ocamlc] ([ocamlc], [ocamldep], [ocamlopt], [ocaml], [ocamlmklib],
[ocamlobjinfo]). Each wrapper restores the original PATH before
exec'ing the real tool, so the build sees the full environment while
dune's PATH-based discovery sees only [ocaml-bin] — which lacks
[ocamlfind]:

  $ mkdir ocaml-bin
  $ cat > ocaml-bin/wrapper <<EOF
  > #!/usr/bin/env sh
  > export PATH="\$ORIG_PATH"
  > exec "\$(basename "\$0")" "\$@"
  > EOF
  $ chmod +x ocaml-bin/wrapper
  $ for tool in ocamlc ocamldep ocamlopt ocaml ocamlmklib ocamlobjinfo; do
  >   ln -s wrapper ocaml-bin/$tool
  > done

  $ DUNE_PATH=$(dirname `which dune`)
  $ SH_PATH=$(dirname `which sh`)

There's nothing in this project to build. The build succeeds without
`ocamlfind` available:

  $ env ORIG_PATH="$PATH" PATH="$SH_PATH:$DUNE_PATH:$PWD/ocaml-bin" dune build
