Check that dune can choose a version of ocamlformat with a suffix (e.g.
0.24+foo) to satisfy a .ocamlformat config that specifies a matching version
without the suffix.
  $ . ./helpers.sh
  $ mkrepo
  $ make_project_with_dev_tool_lockdir

Fake ocamlformat package that appends a comment with the ocamlformat version to the end of the file:
  $ ocamlformat_package() {
  >   cat <<EOF
  > install: [
  >   [ "sh" "-c" "echo '#!/bin/sh' > %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "echo 'cat \$2' >> %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "echo 'echo \$2 | grep .*.ml >/dev/null && echo \"(* formatted with fake ocamlformat %{version}% *)\"' >> %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "chmod a+x %{bin}%/ocamlformat" ]
  > ]
  > EOF
  > }

Make some fake ocamlformat packages:
  $ ocamlformat_package | mkpkg ocamlformat 0.24+foo
  $ ocamlformat_package | mkpkg ocamlformat 0.25+bar

Initial file:
  $ cat foo.ml
  let () = print_endline "Hello, world"

This should choose the 0.24+foo version:
  $ echo "version=0.24" > .ocamlformat
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  Solution for dev-tools.locks/ocamlformat:
  - ocamlformat.0.24+foo
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  Promoting _build/default/.formatted/foo.ml to foo.ml.
  [1]
  $ cat foo.ml
  let () = print_endline "Hello, world"
  (* formatted with fake ocamlformat 0.24+foo *)

This should choose the 0.24+bar version:
  $ echo "version=0.25" > .ocamlformat
  $ rm -rf dev-tools.locks
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  Solution for dev-tools.locks/ocamlformat:
  - ocamlformat.0.25+bar
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  Promoting _build/default/.formatted/foo.ml to foo.ml.
  [1]
  $ cat foo.ml
  let () = print_endline "Hello, world"
  (* formatted with fake ocamlformat 0.24+foo *)
  (* formatted with fake ocamlformat 0.25+bar *)

This should fail as there is no version matching 0.24.1:
  $ echo "version=0.24.1" > .ocamlformat
  $ rm -rf dev-tools.locks
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dev-tools.locks/ocamlformat:
  Can't find all required versions.
  Selected: ocamlformat_dev_tool_wrapper.dev
  - ocamlformat -> (problem)
      ocamlformat_dev_tool_wrapper dev requires >= 0.24.1 & <=
  0.24.1___MAX_VERSION
      Rejected candidates:
        ocamlformat.0.25+bar: Incompatible with restriction: >= 0.24.1 & <=
  0.24.1___MAX_VERSION
        ocamlformat.0.24+foo: Incompatible with restriction: >= 0.24.1 & <=
  0.24.1___MAX_VERSION
  [1]
