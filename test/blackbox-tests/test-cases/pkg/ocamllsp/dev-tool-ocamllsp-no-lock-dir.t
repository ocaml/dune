Exercise the behaviour of "dune tools exec ocamllsp" when run in a
dune project with no lockdir.

Use a mock repository so auto-locking does not access the network.

  $ mkrepo
  $ setup_ocamllsp_workspace

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > 
  > (package
  >  (name foo)
  >  (allow_empty))
  > EOF

Auto-locking finds a solution, but it contains no compiler.

  $ test ! -e dune.lock
  $ dune tools exec ocamllsp
  Error: No compiler declared in the lockfile
  Hint: Add a dependency on a compiler to one of the packages in dune-project
  and then run 'dune build'
  [1]
  $ test -f _build/_private/default/.lock/dune.lock/lock.dune
