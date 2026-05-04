A `-x` cross-compile build no longer requires [ocamlfind] in PATH or
[OCAMLFIND_CONF] set when no findlib library resolution is needed in
the cross-compile context. When a stanza in the cross-compile context
does need an opam-installed library, dune fails with a "Library X not
found" error that points at the offending stanza and context. See
ocaml/dune#10399. (For the case where the cross-compile context goes
entirely unused, see [no-ocamlfind-unused-toolchain-context.t].)

  $ unset OCAMLFIND_TOOLCHAIN
  $ unset OCAMLFIND_CONF

Set up a project with a single private library that has no external
library dependencies, so building it for a cross-compile context
doesn't need findlib:

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > EOF
  $ cat > dune <<EOF
  > (library (name only_stdlib))
  > EOF
  $ cat > only_stdlib.ml <<EOF
  > let x = 42
  > EOF

Wrappers for the OCaml toolchain tools dune resolves as siblings of
[ocamlc] ([ocamlc], [ocamldep], [ocamlopt], [ocaml], [ocamlmklib],
[ocamlobjinfo]). Each wrapper restores the original PATH before
exec'ing the real tool, so the build sees the full environment while
dune's PATH-based discovery sees only [ocaml-bin] — which lacks
[ocamlfind]. Wrapping every sibling keeps the test robust if it
later grows to exercise more compile rules:

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

Cross-compile [-x foo] without ocamlfind. The library only depends on
the stdlib, so no findlib lookup is needed — the build succeeds:

  $ env ORIG_PATH="$PATH" PATH="$SH_PATH:$DUNE_PATH:$PWD/ocaml-bin" dune build -x foo

Now add an opam-resolved library dependency. Library resolution fires
in the cross-compile context, can't find the library (because
ocamlfind is unavailable), and dune reports the offending stanza:

  $ cat > dune <<EOF
  > (library
  >  (name only_stdlib)
  >  (libraries cmdliner))
  > EOF
  $ env ORIG_PATH="$PATH" PATH="$SH_PATH:$DUNE_PATH:$PWD/ocaml-bin" dune build -x foo
  File "dune", line 3, characters 12-20:
  3 |  (libraries cmdliner))
                  ^^^^^^^^
  Error: Library "cmdliner" not found.
  -> required by library "only_stdlib" in _build/default.foo
  -> required by _build/default.foo/.only_stdlib.objs/native/only_stdlib.cmx
  -> required by _build/default.foo/only_stdlib.a
  -> required by alias all (context default.foo)
  -> required by alias default (context default.foo)
  [1]

