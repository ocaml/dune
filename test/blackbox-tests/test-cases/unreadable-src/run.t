  $ mkdir foo && chmod -r foo
  $ dune build
  File "<none>", line 1, characters 0-0:
  Warning: Unable to read directory foo. Ignoring.
  Remove this message by ignoring by adding:
  (dirs \ foo)
  to the dune file: dune
  Reason: foo: Permission denied
  
  $ chmod +r foo && rm -rf foo
