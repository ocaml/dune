Test that after evaling the output of 'dune tools env', the first ocamlmerlin
executable in PATH is the one installed by dune as a dev tool.

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
  Error: The tool ocamlmerlin is not installed.
  Hint: Try running 'dune tools install ocamlmerlin'
  [1]

Now check that 'dune tools env' puts the dev tool in PATH:
  $ eval $(dune tools env)
  $ which ocamlmerlin
  [1]
