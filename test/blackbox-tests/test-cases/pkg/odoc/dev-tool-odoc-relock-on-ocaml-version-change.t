Test that if the version of the "ocaml" package in the project's
lockdir changes then the odoc dev tool is re-locked to be built
with the version of the ocaml compiler now in the project's
lockdir. This is necessary because odoc must be compiled with the
same version of the ocaml compiler as the code that it's analyzing.

  $ . ../helpers.sh
  $ . ./helpers.sh

  $ mkrepo
  $ make_mock_odoc_package
  $ mkpkg ocaml 5.2.0
  $ mkpkg ocaml 5.1.0

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

Initially odoc will be depend on ocaml.5.2.0 to match the project.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune ocaml doc
  Solution for dev-tools.locks/odoc:
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
  $ cat dev-tools.locks/odoc/ocaml.pkg
  (version 5.2.0)

We can re-run "dune ocaml doc" without relocking or rebuilding.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune ocaml doc
  hello from fake odoc
  hello from fake odoc
  File "_doc/_html/_unknown_", line 1, characters 0-0:
  Error: Rule failed to produce directory "_doc/_html/odoc.support"
  File "_doc/_odoc/pkg/foo/_unknown_", line 1, characters 0-0:
  Error: Rule failed to generate the following targets:
  - _doc/_odoc/pkg/foo/page-index.odoc
  [1]

Change the version of ocaml that the project depends on.
  $ make_lockpkg ocaml <<EOF
  > (version 5.1.0)
  > EOF

Running "dune ocaml doc" causes odoc to be relocked and rebuilt
before running. Odoc now depends on ocaml.5.1.0.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune ocaml doc
  The version of the compiler package ("ocaml") in this project's lockdir has
  changed to 5.1.0 (formerly the compiler version was 5.2.0). The dev-tool
  "odoc" will be re-locked and rebuilt with this version of the compiler.
  Solution for dev-tools.locks/odoc:
  - ocaml.5.1.0
  - odoc.0.0.1
  hello from fake odoc
  hello from fake odoc
  File "_doc/_html/_unknown_", line 1, characters 0-0:
  Error: Rule failed to produce directory "_doc/_html/odoc.support"
  File "_doc/_odoc/pkg/foo/_unknown_", line 1, characters 0-0:
  Error: Rule failed to generate the following targets:
  - _doc/_odoc/pkg/foo/page-index.odoc
  [1]
