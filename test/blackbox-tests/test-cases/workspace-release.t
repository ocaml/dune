Here we test how workspace files modify the environment when building a package in and out
of --release mode.

  $ cat > dune-project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name foo))
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (action
  >   (with-outputs-to foo.out
  >    (echo %{env:FOO_VAR=default}))))
  > (install
  >  (files foo.out)
  >  (section share)
  >  (package foo))
  > EOF

Without a workspace file we get the default value.
  $ dune build @install
  $ cat _build/install/default/share/foo/foo.out 
  default

With a workspace file present, we get the value of FOO_VAR set in the workspace file.

  $ cat > dune-workspace <<EOF
  > (lang dune 3.11)
  > (env
  >  (_
  >   (env-vars
  >    (FOO_VAR workspace))))
  > EOF

  $ dune build @install
  $ cat _build/install/default/share/foo/foo.out 
  workspace
 
In --release mode the current behaviour is not to ignore the dune-workspace value.

  $ dune build @install --release
  $ cat _build/install/default/share/foo/foo.out
  workspace
