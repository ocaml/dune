A lock-dir package depends on a workspace package that provides a
public executable.

  $ make_dune_project 3.24
  $ cat >> dune-project <<EOF
  > (package (name ws-tool))
  > EOF

  $ mkdir src
  $ cat > src/dune <<EOF
  > (executable
  >  (name main)
  >  (package ws-tool)
  >  (public_name ws-tool))
  > EOF
  $ cat > src/main.ml <<EOF
  > let () = print_endline "Hello from ws-tool!"
  > EOF

The lock dir contains one package "consumer" that declares "ws-tool"
as a dependency:

  $ make_lockdir
  $ make_lockpkg consumer <<EOF
  > (version 0.0.1)
  > (depends ws-tool)
  > (build (run ws-tool))
  > EOF

A rule depends on the lock-dir package:

  $ write_lockdir_consumer_rule

The locked recipe executes the materialised workspace tool.

  $ dune build out
  Hello from ws-tool!
  $ echo 'let () = print_endline "updated tool"' > src/main.ml
  $ dune build out
  updated tool
