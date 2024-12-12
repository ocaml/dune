With a faulty version of OCamlFormat, "dune fmt" is supposed to stop with the
build error of "ocamlformat".

  $ . ./helpers.sh
  $ mkrepo

Make a fake ocamlformat with a missing ocamlformat.ml file:
  $ make_fake_ocamlformat "0.26.4" "no-ml-file"
  $ make_ocamlformat_opam_pkg "0.26.4"

Make dune-project that uses the mocked dev-tool opam-reposiotry.
  $ make_project_with_dev_tool_lockdir

The project depends on ocaml, so provide a fake ocaml package:
  $ make_ocaml_opam_pkg

The ocamlformat dev tool requires the project to be locked:
  $ dune pkg lock
  Solution for dune.lock:
  - ocaml.0.0.1

It fails during the build because of missing OCamlFormat module.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  Solution for dev-tools.locks/ocamlformat:
  - ocaml.0.0.1
  - ocamlformat.0.26.4
  File "dev-tools.locks/ocamlformat/ocamlformat.pkg", line 4, characters 6-10:
  4 |  (run dune build -p %{pkg-self:name} @install))
            ^^^^
  Error: Logs for package ocamlformat
  File "dune", line 2, characters 14-25:
  2 |  (public_name ocamlformat))
                    ^^^^^^^^^^^
  Error: Module "Ocamlformat" doesn't exist.
  
  [1]
