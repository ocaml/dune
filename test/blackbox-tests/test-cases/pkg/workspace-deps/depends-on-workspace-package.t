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
