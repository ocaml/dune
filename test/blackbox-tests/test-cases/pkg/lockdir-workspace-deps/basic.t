A lock-dir package depends on a workspace library package.

  $ make_workspace_lib_package

The lock dir contains one package "consumer" that declares
"workspace-lib" as a dependency:

  $ make_lockdir
  $ make_lockpkg consumer <<EOF
  > (version 0.0.1)
  > (depends workspace-lib)
  > (build (run echo "building consumer"))
  > EOF

A rule depends on the lock-dir package:

  $ write_lockdir_consumer_rule

The workspace package supplies the missing dependency without a lock entry.

  $ dune build out
  building consumer
  $ cat _build/default/out
  ok

Inactive conditional workspace dependencies do not become materialisation roots.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package
  >  (name workspace-lib)
  >  (depends (absent (= :os "never"))))
  > EOF
  $ dune build out

The same dependency works when the locked consumer is loaded natively.
Its recorded recipe must not execute.

  $ mkdir _consumer
  $ cat > _consumer/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name consumer))
  > EOF
  $ echo '(library (public_name consumer) (libraries workspace-lib))' > _consumer/dune
  $ echo 'let greeting = Workspace_lib.greeting' > _consumer/consumer.ml
  $ make_lockpkg consumer <<EOF
  > (version 0.0.1)
  > (depends workspace-lib)
  > (source (copy $PWD/_consumer))
  > (build (run false))
  > EOF
  $ echo 'let () = print_endline Consumer.greeting' > main.ml
  $ echo '(executable (name main) (libraries consumer))' >> dune
  $ dune exec ./main.exe
  Hello from workspace-lib!

A lock entry for a workspace package is a duplicate definition, not an override.

  $ make_lockpkg workspace-lib <<'EOF'
  > (version 1.0)
  > EOF
  $ dune build out 2>error
  [1]
  $ grep '^Error:' error
  Error: A package named "workspace-lib" is defined locally but is also present
