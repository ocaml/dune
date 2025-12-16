Test that after evaling the output of 'dune tools env', the first ocamlmerlin
executable in PATH is the one installed by dune as a dev tool.

  $ . ../helpers.sh
  $ . ./helpers.sh

  $ mkrepo
  $ make_mock_merlin_package
  $ mk_ocaml 5.2.0

  $ setup_merlin_workspace

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends
  >    (ocaml (= 5.2.0))))
  > EOF

  $ dune build

First install the tool:
  $ dune tools exec ocamlmerlin
  Solution for _build/.dev-tools.locks/merlin:
  - merlin.0.0.1
  - ocaml-base-compiler.5.2.0
  - ocaml-compiler.5.2.0
       Running 'ocamlmerlin'
  hello from fake ocamlmerlin

Now check that 'dune tools env' puts the dev tool in PATH:
  $ eval $(dune tools env)
  $ which ocamlmerlin
  $TESTCASE_ROOT/_build/_private/default/.dev-tool/merlin/target/bin/ocamlmerlin
