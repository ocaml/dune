Reproduction case for #2061. Make sure dune errors out when a rule in
standard mode produces multiple files and only a strict subset of them
is in the source tree.

Before Dune 2.0, Dune was trying to be clever. However, this was more
confusing than anything.

  $ echo '(lang dune 1.10)' > dune-project
  $ dune build a
  Error: Multiple rules generated for _build/default/a:
  - dune:1
  - file present in source tree
  Hint: rm -f a
  [1]
