If the dev-tool feature is enabled then "dune fmt" should invoke the "ocamlformat"
executable from the dev-tool and not the one from PATH.

  $ mkrepo

  $ make_fake_ocamlformat "0.26.2"
  $ make_ocamlformat_opam_pkg "0.26.2"

Make dune-project that uses the mocked dev-tool opam-reposiotry.
  $ make_project_with_dev_tool_lockdir

Change the 'dune' file to use an executable called 'ocamlformat'
  $ cat > dune <<EOF
  > (executable
  >  (public_name foo))
  > (rule
  >  (target none)
  >  (action
  >     (progn
  >       (run ocamlformat foo.ml)
  >       (run touch none))))
  > EOF

Add a fake executable in the PATH
  $ make_fake_ocamlformat_from_path
  $ which ocamlformat
  $TESTCASE_ROOT/.bin/ocamlformat

Build the OCamlFormat binary dev-tool
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt --preview
  Solution for _build/.dev-tools.locks/ocamlformat:
  - ocamlformat.0.26.2
  File "dune", line 1, characters 0-0:
  --- dune
  +++ .formatted/dune
  @@ -1,8 +1,9 @@
   (executable
    (public_name foo))
  +
   (rule
    (target none)
    (action
  -    (progn
  -      (run ocamlformat foo.ml)
  -      (run touch none))))
  +  (progn
  +   (run ocamlformat foo.ml)
  +   (run touch none))))
  File "foo.ml", line 1, characters 0-0:
  --- foo.ml
  +++ .formatted/foo.ml
  @@ -1 +1 @@
  -let () = print_endline "Hello, world"
  +formatted with version 0.26.2
  [1]

When the dev-tool feature is disabled dune runs the OCamlFormat binary from the
PATH and not the dev-tool one.
  $ dune build
  fake ocamlformat from PATH
