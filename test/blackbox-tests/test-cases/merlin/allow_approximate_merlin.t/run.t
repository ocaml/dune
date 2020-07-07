If different options apply to two stanzas in the same directory, the .merlin
file is the union of the two and a warning is emitted.

The output depends on dune-project.

For lang >= 1.9, a warning is printed:

  $ echo '(lang dune 1.9)' > dune-project
  $ dune build @check
  File "dune", line 4, characters 13-26:
  4 |  (preprocess future_syntax))
                   ^^^^^^^^^^^^^
  Warning: .merlin generated is inaccurate. Cannot mix preprocessed and non
  preprocessed specifications.
  Split the stanzas into different directories or silence this warning by
  adding (allow_approximate_merlin) to your dune-project.

Indeed, adding this will suppress the warning:

  $ printf '(lang dune 1.9)\n(allow_approximate_merlin)\n' > dune-project
  $ dune build @check

However, the warning is not emitted if it is not fixable (#2399).

  $ echo '(lang dune 1.8)' > dune-project
  $ dune build @check
