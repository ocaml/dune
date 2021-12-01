  $ mkdir foo && chmod -r foo
  $ dune build
  Warning: Unable to read directory foo. Ignoring.
  Remove this message by ignoring by adding:
  (dirs \ foo)
  to the dune file: dune
  Reason: opendir(foo): Permission denied
  $ chmod +r foo && rm -rf foo
