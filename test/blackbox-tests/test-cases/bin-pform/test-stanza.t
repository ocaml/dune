%{bin:...} in a test stanza adds the binary to PATH.

  $ make_mypkg_bin_project

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
  $ env_added "$(cat _build/default/mytest.exe.output)" "$PATH" | censor
  $PWD/_build/install/default/.binaries/$DIGEST
