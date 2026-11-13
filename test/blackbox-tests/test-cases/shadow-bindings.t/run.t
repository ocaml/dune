Bindings introduced by user dependencies should shadow existing bindings

  $ dune runtest > .#output
  $ sort .#output
  ./foo
  ./xb
