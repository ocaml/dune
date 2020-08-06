  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target some_source)
  >  (action (with-stdout-to %{target} (echo "Hello there!\n"))))
  > \
  > (rule
  >  (target some_copy)
  >  (action
  >   (dynamic-run ./foo.exe)))
  > \
  > (rule
  >  (alias runtest)
  >  (action
  >   (progn
  >    (cat some_source)
  >    (cat some_copy))))
  > EOF

  $ cp ./bin/foo.exe ./

  $ dune runtest
  Hello there!
  Hello there!
