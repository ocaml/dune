A lock-dir package depends on a workspace package.

  $ make_dune_project 3.24
  $ cat >> dune-project <<EOF
  > (package (name myws))
  > EOF

  $ mkdir src
  $ cat > src/dune <<EOF
  > (library (public_name myws))
  > EOF
  $ cat > src/myws.ml <<EOF
  > let x = 1
  > EOF

The lock dir contains one package "mylock" that declares "myws" as a
dependency:

  $ make_lockdir
  $ make_lockpkg mylock <<EOF
  > (version 0.0.1)
  > (depends myws)
  > (build (run echo "building mylock"))
  > EOF

A rule depends on the lock-dir package:

  $ cat > dune <<EOF
  > (rule
  >  (alias check)
  >  (deps (package mylock))
  >  (action (with-stdout-to out (echo "done"))))
  > EOF

The lock entry refers to the existing workspace definition of myws.

  $ dune build @check
  building mylock
  $ cat _build/default/out
  done

A workspace package named dune needs mylock for its development setup. The
opaque recipe's dependency on the dune build tool must not close a cycle back
to the workspace package.

  $ cat >> dune-project <<'EOF'
  > (package
  >  (name dune)
  >  (allow_empty)
  >  (depends (mylock :with-dev-setup)))
  > EOF
  $ cat > dune-workspace <<'EOF'
  > (lang dune 3.26)
  > (lock_dir
  >  (solver_env (with-dev-setup true)))
  > EOF
  $ cat >> dune.lock/lock.dune <<'EOF'
  > (expanded_solver_variable_bindings
  >  (variable_values (with-dev-setup true)))
  > EOF
  $ make_lockpkg mylock <<'EOF'
  > (version 0.0.1)
  > (depends myws dune)
  > (build
  >  (progn
  >   (with-stdout-to dune-version (run dune --version))
  >   (run echo "building mylock with the running dune")))
  > EOF
  $ dune build @check
  Error: Dependency cycle between packages:
     dune
  -> mylock
  -> dune
  -> required by _build/default/.lockfile/pkg/mylock/.opam/mylock/target
  -> required by _build/default/out
  -> required by alias check
  [1]
