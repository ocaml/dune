When installing a dev tool which already has a lockdir, detect the case where
the tool's lockfile is absent from the lockdir and relock.

  $ mkrepo

Make a fake ocamlformat package
  $ make_fake_ocamlformat "0.26.0"
  $ make_ocamlformat_opam_pkg "0.26.0"

  $ make_project_with_dev_tool_lockdir

Install ocamlformat once to generate the lockdir.
  $ dune tools install ocamlformat
  Solution for _build/.dev-tools.locks/ocamlformat
  
  Dependencies common to all supported platforms:
  - ocamlformat.0.26.0

Delete ocamlformat's lockfile.
  $ rm "${dev_tool_lock_dir}"/ocamlformat.pkg
  rm: cannot remove '_build/.dev-tools.locks/ocamlformat/ocamlformat.pkg': No such file or directory
  [1]

Reinstall ocamlformat.
  $ dune tools install ocamlformat
