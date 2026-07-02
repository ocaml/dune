Known limitation: when a package name appears in both the lockdir and
the workspace, the lockdir entry shadows the workspace entry silently.
There is no warning or error, and the workspace package's install
entries are never materialised. The dependency-hash gate
([raise_on_lock_dir_out_of_sync]) catches the usual case where the
workspace changed in a way that requires re-locking, but it operates
on the hash of non-local dependencies and does not detect a hand-edit
that introduces a same-named entry without altering the hash. For
solver-generated lockdirs the collision cannot arise. This test pins
down the current behaviour so that any future change is intentional.

A workspace package "shared" defines a library:

  $ make_dune_project_with_package 3.24 shared
  $ mkdir src
  $ cat > src/dune <<EOF
  > (library (name shared) (public_name shared))
  > EOF
  $ cat > src/shared.ml <<EOF
  > let from_workspace = "ws"
  > EOF

The lock dir contains a package also called "shared":

  $ make_lockdir
  $ make_lockpkg shared <<EOF
  > (version 0.0.1)
  > (build (run echo "building shared from lockdir"))
  > EOF

A consumer lock-dir package depends on "shared":

  $ make_lockpkg consumer <<EOF
  > (version 0.0.1)
  > (depends shared)
  > (build (run echo "building consumer"))
  > EOF

  $ write_lockdir_consumer_rule

The lockdir build of "shared" runs, demonstrating that the resolver
picked the lockdir entry. No install layout is materialised for
"shared" - it would have contained the workspace's [shared.cmi] and
friends had the workspace entry won. There is no diagnostic to alert
the user that their workspace package was shadowed.

  $ dune build out
  building shared from lockdir
  building consumer

  $ find _build/install/default/.packages -type f -o -type l | censor | sort
  find: '_build/install/default/.packages': No such file or directory
  
  [1]
