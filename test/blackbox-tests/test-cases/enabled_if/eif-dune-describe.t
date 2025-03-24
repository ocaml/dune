  $ cat > dune <<EOF
  > (executable
  >  (name test)
  >  (enabled_if
  >   (= %{system} unknown)))
  > (rule
  >  (enabled_if
  >   (= %{system} unknown))
  >  (alias runtest)
  >  (action
  >  (run ./test.exe)))
  > EOF

  $ cat > test.ml <<EOF
  > let () = print_string "Hello world"
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name pack)
  >  (allow_empty))
  > EOF

  $ dune build
  $ dune runtest
  $ dune describe 2>&1
  ((root
    $TESTCASE_ROOT)
   (build_context _build/default))
