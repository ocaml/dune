TODO this test is now obsolete

If different options apply to two stanzas in the same directory, the .merlin
file is the union of the two and a warning is emitted.

The output depends on dune-project.

For lang >= 1.9, a warning is printed:

  $ echo '(lang dune 1.9)' > dune-project
  $ dune build @check

Indeed, adding this will suppress the warning:

  $ printf '(lang dune 1.9)\n(allow_approximate_merlin)\n' > dune-project
  $ dune build @check

However, the warning is not emitted if it is not fixable (#2399).

  $ echo '(lang dune 1.8)' > dune-project
  $ dune build @check
