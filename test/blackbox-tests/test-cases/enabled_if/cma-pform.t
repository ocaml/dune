We try to build a disabled library using the %{cma:..} pfrom

  $ cat >dune-project <<EOF
  > (lang dune 3.14)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (enabled_if (= with-foo %{profile})))
  > EOF

  $ dune build %{cma:./foo}
  File "command line", line 1, characters 0-12:
  Error: Library foo does not exist.
  [1]
  $ dune build --profile with-foo %{cma:./foo}
