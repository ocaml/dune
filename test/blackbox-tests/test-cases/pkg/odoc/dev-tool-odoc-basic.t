Test that the "dune ocaml doc" command causes odoc to be
locked, built and run when the command is run from a dune project with
a lockdir containing an "ocaml" lockfile.

  $ . ../helpers.sh
  $ . ./helpers.sh

  $ mkrepo
  $ make_mock_odoc_package
  $ mkpkg ocaml 5.2.0

  $ setup_odoc_workspace

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty))
  > EOF

  $ make_lockdir
  $ make_lockpkg ocaml <<EOF
  > (version 5.2.0)
  > EOF

  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune ocaml doc
  Solution for .dune-tools-solution-cache/odoc:
  - ocaml.5.2.0
  - odoc.0.0.1
  hello from fake odoc
  hello from fake odoc
  File "_doc/_html/_unknown_", line 1, characters 0-0:
  Error: Rule failed to produce directory "_doc/_html/odoc.support"
  File "_doc/_odoc/pkg/foo/_unknown_", line 1, characters 0-0:
  Error: Rule failed to generate the following targets:
  - _doc/_odoc/pkg/foo/page-index.odoc
  [1]
