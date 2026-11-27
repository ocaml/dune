A lock-dir package depends on a workspace package.

  $ make_dune_project_with_package 3.24 myws

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
  >   (run dune --version)
  >   (run echo "building mylock with the running dune")))
  > EOF
  $ dune build @check >build-output 2>&1
  $ grep '^building mylock' build-output
  building mylock with the running dune

The same tool dependency must not expose workspace development dependencies
when resolving the libraries of a mounted package.

  $ mkdir _native
  $ (cd _native && make_dune_project_with_package 3.26 native)
  $ cat > _native/dune <<'EOF'
  > (library
  >  (public_name native)
  >  (libraries compiler-libs.common))
  > EOF
  $ echo 'let value = 1' > _native/native.ml
  $ make_dune_source_lockpkg native native "$PWD/_native"
  $ echo '(depends dune)' | append_to_lockpkg native
  $ make_lockpkg mylock <<'EOF'
  > (version 0.0.1)
  > (depends myws native dune)
  > (build (run echo "building mylock with native"))
  > EOF
  $ build_pkg mylock 2>&1 | censor
  building mylock with native
