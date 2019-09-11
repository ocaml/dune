  $ echo "(lang dune 2.0)" > dune-project

  $ cat > dune << EOF
  > (rule
  >  (target bar_source)
  >  (action (with-stdout-to %{target} (echo "Hello from bar!\n"))))
  > \
  > (rule
  >  (target bar_copy)
  >  (action
  >   (dynamic-run ./foo.exe)))
  > \
  > (alias
  >  (name runtest)
  >  (action
  >   (progn
  >    (cat bar_source)
  >    (cat bar_copy))))
  > EOF

  $ cp ../bin/foo.exe ./

  $ dune runtest
  Hello from bar!
  Hello from bar!
