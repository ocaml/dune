After the parsing non-ascii characters could be compared through expressions.
Linked to the issue #9728

  $ echo '(lang dune 3.0)' > dune-project
  $ cat >dune <<"EOF"
  > (rule
  >  (alias default)
  >  (enabled_if (= "É" "\195\137"))
  >  (action (echo "Hello, world!\n")))
  > EOF

  $ dune build
  Hello, world!
