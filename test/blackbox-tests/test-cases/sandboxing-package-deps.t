Test sandboxing when depending on things from the install context using
(package ..).

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (package (name foo))
  > EOF

  $ mkdir foo/
  $ cat >foo/dune <<EOF
  > (executable
  >  (package foo)
  >  (public_name mybin))
  > EOF
  $ cat >foo/mybin.ml <<EOF
  > print_endline "hello from package foo";;
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (deps (sandbox always) (package foo))
  >  (action (system "pwd; command -v mybin; echo %{bin:mybin}; mybin")))
  > EOF

  $ dune build --sandbox symlink @foo 2>&1 | sed -E 's#.*.sandbox/[^/]+/#.sandbox/$SANDBOX/#g'
  .sandbox/$SANDBOX/default
  .sandbox/$SANDBOX/default/test/blackbox-tests/test-cases/_build/install/default/bin/mybin
  ../install/default/bin/mybin
  hello from package foo
