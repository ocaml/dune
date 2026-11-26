Demonstrate that rule stanza is now sandboxed by default

Use default sandbox preference. The test suite sets `DUNE_SANDBOX`, so clear it.

  $ unset DUNE_SANDBOX
  $ make_dune_project 3.22

  $ cat > dune <<EOF
  > (executable
  >  (name probe))
  > (rule
  >  (target result)
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{exe:probe.exe}))))
  > EOF

  $ cat > probe.ml <<EOF
  > let () = print_endline (string_of_bool (Sys.file_exists "probe.ml"))
  > EOF

  $ dune build result
  $ cat _build/default/result
  true

  $ make_dune_project 3.23
  $ rm -rf _build
  $ dune build result
  $ cat _build/default/result
  false

An explicit request to disable sandboxing conflicts with the default.

  $ cat > dune <<EOF
  > (executable
  >  (name probe))
  > (rule
  >  (target result)
  >  (deps (sandbox none))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{exe:probe.exe}))))
  > EOF
  $ rm -rf _build
  $ dune build result
  File "dune", lines 3-8, characters 0-111:
  3 | (rule
  4 |  (target result)
  5 |  (deps (sandbox none))
  6 |  (action
  7 |   (with-stdout-to %{target}
  8 |    (run %{exe:probe.exe}))))
  Error: This rule forbids all sandboxing modes (but it also requires
  sandboxing)
  [1]

The command-line sandbox preference cannot disable sandboxing either.

  $ cat > dune <<EOF
  > (executable
  >  (name probe))
  > (rule
  >  (target result)
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{exe:probe.exe}))))
  > EOF
  $ rm -rf _build
  $ dune build --sandbox=none result
  $ cat _build/default/result
  false
