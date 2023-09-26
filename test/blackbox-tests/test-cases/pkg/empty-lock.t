An empty dune.lock directory should prevent dune from building. We should also
give a nice error message telling the user what is wrong and how it can be
fixed.

  $ mkdir dune.lock
  $ cat > dune-project <<EOF
  > (lang dune 3.11)
  > EOF
  $ dune build
  Error: dune.lock/lock.dune: No such file or directory
  [1]
