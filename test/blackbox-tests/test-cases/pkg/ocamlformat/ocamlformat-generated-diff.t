Generated diff corrections use the locked OCamlFormat dev tool.

  $ mkrepo
  $ make_fake_ocamlformat "0.26.2"
  $ make_ocamlformat_opam_pkg "0.26.2"
  $ make_project_with_dev_tool_lockdir
  $ dune_cmd subst '3.13' '3.25' dune-project
  $ cat >> dune <<'EOF'
  > (rule
  >  (alias generate)
  >  (action
  >   (progn
  >    (with-stdout-to foo.ml.generated
  >     (echo "let generated = 1"))
  >    (diff? foo.ml foo.ml.generated))))
  > EOF

  $ dune tools install ocamlformat
  Solution for _build/.dev-tools.locks/ocamlformat:
  - ocamlformat.0.26.2

  $ dune build @generate --auto-promote
  File "foo.ml", line 1, characters 0-0:
  --- foo.ml
  +++ foo.ml.generated
  @@ -1 +1 @@
  -let () = print_endline "Hello, world"
  +formatted with version 0.26.2
  Promoting _build/default/foo.ml.generated to foo.ml.
  [1]

  $ cat foo.ml
  formatted with version 0.26.2
