  $ echo '(lang dune 1.0)' > dune-project
  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries
  >   (select bar.ml from
  >    (unix -> bar.unix.ml)
  >    (!unix -> bar.no_unix.ml))
  >   (select foo.ml from
  >    (fakefoobar -> foo.fake.ml)
  >    (!fakefoobar -> foo.no_fake.ml))))
  > (alias
  >  (name runtest)
  >  (action (run ./main.exe)))
  > EOF

  $ dune runtest
  bar has unix
  foo has no fake

  $ echo '(lang dune 2.0)' > dune-project
  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries
  >   (select bar.ml from
  >    (unix -> bar.unix.ml)
  >    (!unix -> bar.no_unix.ml))
  >   (select foo.ml from
  >    (fakefoobar -> foo.fake.ml)
  >    (!fakefoobar -> foo.no_fake.ml))))
  > (rule
  >  (alias runtest)
  >  (action (run ./main.exe)))
  > EOF

  $ dune runtest
  bar has unix
  foo has no fake
