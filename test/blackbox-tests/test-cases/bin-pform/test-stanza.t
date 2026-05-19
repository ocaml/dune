%{bin:...} in a test stanza adds the binary to PATH.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name mypkg))
  > EOF
  $ mkdir src
  $ cat >src/dune <<'EOF'
  > (executable (public_name mybin) (package mypkg))
  > EOF
  $ cat >src/mybin.ml <<'EOF'
  > let () = print_endline "hello from mybin"
  > EOF

  $ cat >dune <<'EOF'
  > (test
  >  (name mytest)
  >  (deps %{bin:mybin}))
  > EOF

  $ cat >mytest.ml <<EOF
  > let () = print_string (Sys.getenv "PATH")
  > EOF

The test prints PATH to stdout. An empty mytest.expected forces the
diff to fail (exit 1) and dune writes the actual stdout to
mytest.exe.output, which we then read.

  $ cat >mytest.expected

  $ dune runtest 2>/dev/null
  [1]
  $ env_added "$(cat _build/default/mytest.exe.output)" "$PATH"
  $TESTCASE_ROOT/_build/install/default/bin
