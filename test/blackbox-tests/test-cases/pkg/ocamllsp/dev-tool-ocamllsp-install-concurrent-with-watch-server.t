Test that ocamllsp can be installed while dune is running in watch mode.

  $ mkrepo
  $ make_mock_ocamllsp_package
  $ mkpkg ocaml 5.2.0

  $ setup_ocamllsp_workspace

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > 
  > (package
  >  (name foo))
  > EOF

  $ make_lockdir
  $ make_lockpkg ocaml <<EOF
  > (version 5.2.0)
  > EOF

  $ cat > foo.ml <<EOF
  > let () = print_endline "hi"
  > EOF

  $ cat > dune <<EOF
  > (executable (public_name foo))
  > EOF

  $ dune build --watch &
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...

  $ dune tools exec ocamllsp
  Solution for dev-tools.locks/ocaml-lsp-server:
  - ocaml.5.2.0
  - ocaml-lsp-server.0.0.1
       Running 'ocamllsp'
  hello from fake ocamllsp

  $ dune shutdown
  $ wait
