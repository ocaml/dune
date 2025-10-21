When installing a dev tool which already has a lockdir, detect the case where
the tool's lockfile is absent from the lockdir and relock.

  $ . ./helpers.sh
  $ mkrepo

Make a fake ocamlformat package
  $ make_fake_ocamlformat "0.26.0"
  $ make_ocamlformat_opam_pkg "0.26.0"

  $ make_project_with_dev_tool_lockdir

Install ocamlformat once to generate the lockdir.
  $ dune tools install ocamlformat
  Solution for dev-tools.locks/ocamlformat:
  - ocamlformat.0.26.0

Delete ocamlformat's lockfile.
  $ rm dev-tools.locks/ocamlformat/ocamlformat.pkg

Reinstall ocamlformat.
  $ dune tools install ocamlformat
  Warning: The lock directory for the tool "ocamlformat" exists but does not
  contain a lockfile for the package "ocamlformat". This may indicate that the
  lock directory has been tampered with. Please avoid making manual changes to
  tool lock directories. The tool will now be relocked.
  Solution for dev-tools.locks/ocamlformat:
  - ocamlformat.0.26.0
