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
