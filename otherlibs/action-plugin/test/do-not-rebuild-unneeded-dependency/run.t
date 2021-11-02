This test checks that in case the dependency of multi staged computation changes,
only the dependencies up to this stage are rebuilt.

  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (deps   bar_source)
  >  (target bar)
  >  (action
  >   (progn
  >    (echo "Building bar!\n")
  >    (copy %{deps} %{target}))))
  > \
  > (rule
  >  (deps   foo_source)
  >  (target foo)
  >  (action
  >   (progn
  >    (echo "Building foo!\n")
  >    (copy %{deps} %{target}))))
  > \
  > (rule
  >  (deps   foo_or_bar_source)
  >  (target foo_or_bar)
  >  (action
  >   (progn
  >    (echo "Building foo_or_bar!\n")
  >    (copy %{deps} %{target}))))
  > \
  > (rule
  >  (alias runtest)
  >  (action (dynamic-run ./client.exe)))
  > EOF

  $ cp ./bin/client.exe ./

  $ printf "foo" > foo_or_bar_source
  $ printf "Hello from foo!" > foo_source
  $ printf "SHOULD NOT BE PRINTED!" > bar_source

  $ dune runtest
  Building foo_or_bar!
  Building foo!
  Hello from foo!

  $ printf "bar" > foo_or_bar_source
  $ printf "SHOULD NOT BE PRINTED!" > foo_source
  $ printf "Hello from bar!" > bar_source

  $ dune runtest
  Building foo_or_bar!
  Building bar!
  Hello from bar!
