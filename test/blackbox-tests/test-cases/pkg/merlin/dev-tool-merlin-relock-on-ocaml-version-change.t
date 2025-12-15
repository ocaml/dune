Test that if the version of the "ocaml" package in the project's
lockdir changes then the merlin dev tool is re-locked to be built
with the version of the ocaml compiler now in the project's
lockdir. This is necessary because merlin must be compiled with the
same version of the ocaml compiler as the code that it's analyzing.

  $ . ../helpers.sh
  $ . ./helpers.sh

  $ mkrepo
  $ make_mock_merlin_package
  $ mkpkg ocaml 5.2.0
  $ mkpkg ocaml 5.1.0

  $ setup_merlin_workspace

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

Initially merlin will depend on ocaml.5.2.0 to match the project.
  $ dune tools exec merlin
  Solution for _build/.dev-tools.locks/merlin:
  - merlin.0.0.1
  - ocaml.5.2.0
       Running 'merlin'
  hello from fake ocamlmerlin
  $ cat "${dev_tool_lock_dir}"/ocaml.pkg
  (version 5.2.0)

We can re-run "dune tools exec merlin" without relocking or rebuilding.
  $ dune tools exec merlin
       Running 'merlin'
  hello from fake ocamlmerlin

Change the version of ocaml that the project depends on.
  $ make_lockpkg ocaml <<EOF
  > (version 5.1.0)
  > EOF

Running "dune tools exec merlin" causes merlin to be relocked and rebuilt
before running. Merlin now depends on ocaml.5.1.0.
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune tools exec merlin
  The version of the compiler package ("ocaml") in this project's lockdir has
  changed to 5.1.0 (formerly the compiler version was 5.2.0). The dev-tool
  "merlin" will be re-locked and rebuilt with this version of the compiler.
  Solution for _build/.dev-tools.locks/merlin:
  - merlin.0.0.1
  - ocaml.5.1.0
       Running 'merlin'
  hello from fake ocamlmerlin
  $ cat "${dev_tool_lock_dir}"/ocaml.pkg
  (version 5.1.0)
