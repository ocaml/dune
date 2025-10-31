Test that ocamlformat is re-run when a source file changes.

  $ . ./helpers.sh
  $ mkrepo
  $ make_project_with_dev_tool_lockdir

Make a fake ocamlformat package that appends a comment to the end of its input.
  $ mkpkg ocamlformat <<EOF
  > install: [
  >   [ "sh" "-c" "echo '#!/bin/sh' > %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "echo 'cat \$2' >> %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "echo 'echo \$2 | grep .*.ml >/dev/null && echo \"(* formatted with fake ocamlformat *)\"' >> %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "chmod a+x %{bin}%/ocamlformat" ]
  > ]
  > EOF

Initial file:
  $ cat foo.ml
  let () = print_endline "Hello, world"

  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  Solution for .dune-tools-solution-cache/ocamlformat:
  - ocamlformat.0.0.1
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  Promoting _build/default/.formatted/foo.ml to foo.ml.
  [1]

After formatting the fake ocamlformat has added a suffix:
  $ cat foo.ml
  let () = print_endline "Hello, world"
  (* formatted with fake ocamlformat *)

Update the file:
  $ cat > foo.ml <<EOF
  > let () = print_endline "Hello, ocaml!"
  > EOF

  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  File "foo.ml", line 1, characters 0-0:
  Error: Files _build/default/foo.ml and _build/default/.formatted/foo.ml
  differ.
  Promoting _build/default/.formatted/foo.ml to foo.ml.
  [1]

The update to the file persists after formatting it a second time:
  $ cat foo.ml
  let () = print_endline "Hello, ocaml!"
  (* formatted with fake ocamlformat *)
