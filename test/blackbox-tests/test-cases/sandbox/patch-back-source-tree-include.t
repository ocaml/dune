Patch-back sandboxing from an included deps file is treated like direct patch-back

  $ make_dune_project 3.23

  $ touch mod

  $ cat > deps.inc <<'EOF'
  > ((sandbox patch_back_source_tree))
  > EOF

  $ cat > dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (deps mod (include deps.inc))
  >  (action (bash "echo foo > mod")))
  > EOF

  $ dune runtest
  File "mod", line 1, characters 0-0:
  --- mod
  +++ _build/default/mod
  @@ -0,0 +1 @@
  +foo
  [1]

  $ cat > dune <<'EOF'
  > (rule
  >  (alias runtest)
  >  (deps mod (sandbox patch_back_source_tree))
  >  (action (bash "echo foo > mod")))
  > EOF

  $ dune runtest
  File "mod", line 1, characters 0-0:
  --- mod
  +++ _build/default/mod
  @@ -0,0 +1 @@
  +foo
  [1]

  $ make_dune_project 3.22

  $ dune runtest
  File "mod", line 1, characters 0-0:
  --- mod
  +++ _build/default/mod
  @@ -0,0 +1 @@
  +foo
  [1]
