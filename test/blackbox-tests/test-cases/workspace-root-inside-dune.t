Tests for workspace root discovery when INSIDE_DUNE is set (issue #5785).

When running inside a dune action (where INSIDE_DUNE is set), root
discovery should not escape the sandbox boundary by traversing up to
parent directories. If no dune-project exists at the cwd, the cwd is
used as the root (which results in an empty build with no errors).

Running dune build from a directory without a dune-project while inside
dune uses the directory as root and completes without error.

  $ mkdir noproject
  $ (cd noproject && dune build 2>&1); echo "exit: $?"
  exit: 0
