A lock-dir package depends on a workspace library that itself depends on
another lock-dir package. This is the full in-and-out graph:

consumer (lock dir) -> workspace-lib (workspace) -> lock-base (lock dir)

First create the library installed by lock-base. It only needs bytecode
artifacts for this test.

  $ mkdir lock-base-src
  $ cat > lock-base-src/lock_base.ml <<EOF
  > let greeting = "Hello through lock-base!"
  > EOF
  $ (cd lock-base-src && \
  >   ocamlc -c lock_base.ml && \
  >   ocamlc -a -o lock_base.cma lock_base.cmo)
  $ cat > lock-base-src/META <<EOF
  > version = "0.0.1"
  > description = ""
  > archive(byte) = "lock_base.cma"
  > EOF
  $ cat > lock-base-src/lock-base.install <<EOF
  > lib: [
  >  "META"
  >  "lock_base.cma"
  >  "lock_base.cmi"
  > ]
  > EOF

The workspace package depends on lock-base both in its package metadata and
in the library stanza. Its installed META therefore requires lock-base.

  $ make_dune_project 3.24
  $ cat >> dune-project <<EOF
  > (package
  >  (name workspace-lib)
  >  (depends lock-base))
  > EOF
  $ mkdir src
  $ cat > src/dune <<EOF
  > (library
  >  (name workspace_lib)
  >  (public_name workspace-lib)
  >  (libraries lock-base)
  >  (modes byte))
  > EOF
  $ cat > src/workspace_lib.ml <<EOF
  > let greeting = Lock_base.greeting
  > EOF

The lock dir contains lock-base and consumer. Consumer declares only its
immediate dependency on workspace-lib, as it would in an opam package.

  $ make_lockdir
  $ make_lockpkg lock-base <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/lock-base-src))
  > EOF
  $ make_lockpkg consumer <<'EOF'
  > (version 0.0.1)
  > (depends workspace-lib)
  > (build
  >  (progn
  >   (system "echo 'let () = print_endline' > consumer.ml")
  >   (system "echo '  Workspace_lib.greeting' >> consumer.ml")
  >   (run
  >    ocamlfind
  >    ocamlc
  >    -package
  >    workspace-lib
  >    -linkpkg
  >    consumer.ml
  >    -o
  >    consumer.exe)
  >   (run ./consumer.exe)))
  > EOF

  $ write_lockdir_consumer_rule

The mixed dependency closure makes both workspace-lib and lock-base visible to
consumer without making consumer visible while workspace-lib is built.

  $ dune build out 2>&1 | censor
  Hello through lock-base!
