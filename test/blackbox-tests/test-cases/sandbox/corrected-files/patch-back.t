Patch-back sandboxing only allows corrections ignore

  $ make_dune_project 3.23
  $ cat > dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (corrections ignore)
  >  (deps (sandbox patch_back_source_tree))
  >  (action (system "true")))
  > EOF
  $ dune build @runtest && echo passed
  passed
  $ cat > dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (corrections produce)
  >  (deps (sandbox patch_back_source_tree))
  >  (action (system "true")))
  > EOF
  $ dune build @runtest
  File "dune", lines 1-5, characters 0-113:
  1 | (rule
  2 |  (alias runtest)
  3 |  (corrections produce)
  4 |  (deps (sandbox patch_back_source_tree))
  5 |  (action (system "true")))
  Error: Only (corrections ignore) is allowed on rules using
  patch-back-source-tree sandboxing.
  [1]
