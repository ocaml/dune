Check that empty variables are treated the same as undefined

  $ cat >dune-project <<EOF
  > (lang dune 2.7)
  > EOF

  $ cat >dune <<EOF
  > (rule (alias doit) (action (echo "%{env:FOO=bar}\n")))
  > EOF

  $ FOO=123 dune build @doit
  123
  $ FOO= dune build @doit
  bar

  $ cat >dune <<EOF
  > (rule (alias doit) (action (setenv FOO "" (echo %{env:FOO=bar}))))
  > EOF

  $ dune build @doit
  bar
