Test that if the version of the "ocaml" package in the project's
lockdir changes then the merlin dev tool is re-locked to be built
with the version of the ocaml compiler now in the project's
lockdir. This is necessary because merlin must be compiled with the
same version of the ocaml compiler as the code that it's analyzing.

  $ . ../helpers.sh
  $ . ./helpers.sh

  $ mkrepo
  $ make_mock_merlin_package
  $ mk_ocaml 5.2.0
  $ mk_ocaml 5.1.0

  $ setup_merlin_workspace

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >  (ocaml (= 5.2.0))))
  > EOF

  $ dune build

Initially merlin will depend on ocaml-base-compiler.5.2.0 to match the project.

  $ dune tools exec ocamlmerlin
  Solution for _build/.dev-tools.locks/merlin:
  - merlin.0.0.1
  - ocaml-base-compiler.5.2.0
  - ocaml-compiler.5.2.0
       Running 'ocamlmerlin'
  hello from fake ocamlmerlin
  $ grep "version" "${dev_tool_lock_dir}"/ocaml-base-compiler.pkg
  (version 5.2.0)

We can re-run "dune tools exec ocamlmerlin" without relocking or rebuilding.
  $ dune tools exec ocamlmerlin
  The version of the compiler package ("ocaml-base-compiler") in this project's
  lockdir has changed to 5.2.0 (formerly the compiler version was 5.2.0). The
  dev-tool "merlin" will be re-locked and rebuilt with this version of the
  compiler.
  Solution for _build/.dev-tools.locks/merlin:
  - merlin.0.0.1
  - ocaml-base-compiler.5.2.0
  - ocaml-compiler.5.2.0
       Running 'ocamlmerlin'
  hello from fake ocamlmerlin

Change the version of ocaml that the project depends on.

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >  (ocaml (= 5.1.0))))
  > EOF

  $ dune build

Running "dune tools exec ocamlmerlin" causes merlin to be relocked and rebuilt
before running. Merlin now depends on ocaml.5.1.0.
  $ dune tools exec ocamlmerlin
  The version of the compiler package ("ocaml-base-compiler") in this project's
  lockdir has changed to 5.1.0 (formerly the compiler version was 5.2.0). The
  dev-tool "merlin" will be re-locked and rebuilt with this version of the
  compiler.
  Solution for _build/.dev-tools.locks/merlin:
  - merlin.0.0.1
  - ocaml-base-compiler.5.1.0
  - ocaml-compiler.5.1.0
       Running 'ocamlmerlin'
  hello from fake ocamlmerlin
  $ grep "version" "${dev_tool_lock_dir}"/ocaml-base-compiler.pkg
  (version 5.1.0)
