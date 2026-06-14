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
  > (build (run echo "building consumer"))
  > EOF

A rule depends on the lock-dir package:

  $ write_lockdir_consumer_rule

The build succeeds. The workspace executable appears under the layout's
bin/ section:

  $ dune build out
  building consumer

  $ find _build/install/default/.packages -type f -o -type l | censor | sort
  _build/install/default/.packages/$DIGEST/bin/ws-tool
  _build/install/default/.packages/$DIGEST/lib/ws-tool/META
  _build/install/default/.packages/$DIGEST/lib/ws-tool/dune-package
