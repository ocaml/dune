Test %{profile} in enabled_if on executables

  $ cat >dune-project <<EOF
  > (lang dune 3.14)
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name foo)
  >  (enabled_if (= %{profile} foo)))
  > EOF

  $ touch foo.ml

  $ dune build ./foo.exe
  Error: Don't know how to build ./foo.exe
  [1]
  $ dune build --profile foo ./foo.exe
