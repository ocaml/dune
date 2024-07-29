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
  $ dune describe 2>&1 | head -n 5
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("modules_and_obj_dir: failed lookup",
    { keys = []; for_ = Exe { first_exe = "test" } })
  Raised at Stdune__Code_error.raise in file
