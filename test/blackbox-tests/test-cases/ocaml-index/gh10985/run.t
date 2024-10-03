This test attempts to build the ocaml index while depending on a library
installed through a lock file.

This is based off of the pkg/libraries.t test, just taking advantage of
being in its own directory.

This can't be just written to disk, I /think/ so it cat get an absolute path
in the source stanza.

  $ cat >dune.lock/mypkg.pkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/external_sources))
  > (build (run dune build --release --promote-install-file=true . @install))
  > EOF

  $ dune build @ocaml-index
  Error: This rule defines a directory target "default/.pkg/mypkg/target" that
  matches the requested path
  "default/.pkg/mypkg/target/lib/mypkg/lib/cctx.ocaml-index" but the rule's
  action didn't produce it
  -> required by _build/default/.foo.objs/cctx.ocaml-index
  -> required by alias ocaml-index
  [1]
