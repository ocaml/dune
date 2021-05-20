%{exe:foo.exe} should not be veisible if foo.exe is added to PATH via the
binaries stanza. %{bin:foo} is visible on the other hand.
  $ dune build
  Error: No rule found for foo.exe
  -> required by alias default in dune:5
           foo alias default
  this is foo.exe
  [1]
