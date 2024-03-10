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
  Error: No rule found for foo.cma
  -> required by %{cma:./foo} at command line:1
  [1]
  $ dune build --profile with-foo %{cma:./foo}
