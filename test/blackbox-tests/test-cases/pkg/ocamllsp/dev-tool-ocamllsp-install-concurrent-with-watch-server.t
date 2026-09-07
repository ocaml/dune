Exercise installing ocamllsp while dune is running in watch mode.

  $ mkrepo
  $ make_mock_ocamllsp_package
  $ mk_ocaml 5.2.0

  $ setup_ocamllsp_workspace

  $ make_named_package_project foo 3.18 "(ocaml (= 5.2.0))"
  $ dune build

  $ cat > foo.ml <<EOF
  > let () = print_endline "hi"
  > EOF

  $ cat > dune <<EOF
  > (executable (public_name foo))
  > EOF

  $ dune build --watch >.#watch-output 2>&1 &
  $ dune rpc ping --wait
  Server appears to be responding normally
  $ wait_for_line_with_timeout .#watch-output \
  >   "Success, waiting for filesystem changes..." 200

BUG: Installing the tool hangs after locking while the watch server is running.
Bound the wait so the test can still check the result and shut down the server.

  $ $timeout 10 dune tools exec ocamllsp
  Solution for _build/.dev-tools.locks/ocaml-lsp-server:
  - ocaml-base-compiler.5.2.0
  - ocaml-compiler.5.2.0
  - ocaml-lsp-server.0.0.1
  [124]

The server is still responsive, but the tool has not been installed.

  $ dune rpc ping
  Server appears to be responding normally
  $ dune tools which ocamllsp
  Error: ocamllsp is not installed as a dev tool
  [1]

  $ dune shutdown
  $ wait
