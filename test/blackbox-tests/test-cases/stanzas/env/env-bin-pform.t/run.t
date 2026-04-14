%{exe:foo.exe} should not be veisible if foo.exe is added to PATH via the
binaries stanza. %{bin:foo} is visible on the other hand.
  $ dune build
  this is foo.exe
  Error: No rule found for foo.exe
  -> required by %{exe:foo.exe} at dune:7
  -> required by alias default in dune:5
  [1]
