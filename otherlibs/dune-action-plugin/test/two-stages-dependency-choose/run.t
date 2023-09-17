In this test client choose what to depend
on based on dependency from the previous stage.

  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target bar)
  >  (action
  >   (progn
  >    (echo "Building bar!\n")
  >    (with-stdout-to %{target} (echo "Hello from bar!")))))
  > \
  > (rule
  >  (target foo)
  >  (action
  >   (progn
  >    (echo "SHOULD NOT BE PRINTED!\n")
  >    (with-stdout-to %{target} (echo "Hello from foo!")))))
  > \
  > (rule
  >  (target foo_or_bar)
  >  (action (with-stdout-to %{target} (echo "bar"))))
  > \
  > (rule
  >  (alias runtest)
  >  (action (dynamic-run ./client.exe)))
  > EOF

  $ cp ./bin/client.exe ./

  $ dune runtest
  Building bar!
  Hello from bar!
