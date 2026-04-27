Test sandboxing when depending on things from the install context using
(package ...).

  $ cat >dune-project <<EOF
  > (lang dune 3.16)
  > (package (name foo))
  > EOF
  $ mkdir foo
  $ cat >foo/dune <<EOF
  > (executable
  >  (public_name mybin))
  > EOF
  $ cat >foo/mybin.ml <<EOF
  > print_endline "hello from package foo";;
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (deps (sandbox always) (package foo))
  >  (action (bash "pwd; command -v mybin; echo %{bin:mybin}; mybin")))
  > EOF

  $ dune build --sandbox symlink @foo 2>&1 | censor
  $PWD/_build/.sandbox/$DIGEST1/default
  $PWD/_build/install/default/.packages/$DIGEST2/bin/mybin
  foo/mybin.exe
  hello from package foo
