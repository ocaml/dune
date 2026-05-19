%{bin:...} in cram (deps ...) makes the binary callable by name.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name mypkg))
  > EOF
  $ mkdir src tests
  $ cat >src/dune <<'EOF'
  > (executable (public_name mybin) (package mypkg))
  > EOF
  $ cat >src/mybin.ml <<'EOF'
  > let () = print_endline "hello from mybin"
  > EOF

  $ cat >tests/dune <<'EOF'
  > (cram (deps %{bin:mybin}))
  > EOF
  $ cat >tests/run.t <<'EOF'
  >   $ mybin
  > EOF

  $ dune runtest tests
  File "tests/run.t", line 1, characters 0-0:
  --- tests/run.t
  +++ tests/run.t.corrected
  @@ -1 +1,2 @@
     $ mybin
  +  hello from mybin
  [1]
