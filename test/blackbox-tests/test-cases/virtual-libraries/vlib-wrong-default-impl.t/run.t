Check that dune makes a proper error if the default implementation of a virtual
library is not actually an implementation of the virtual library.

  $ dune build @default
  Error: "not_an_implem" is not an implementation of "vlibfoo".
  -> required by executable exe in exe/dune:2
  -> required by _build/default/exe/.exe.eobjs/byte/dune__exe__Exe.cmi
  -> required by _build/default/exe/.exe.eobjs/native/dune__exe__Exe.cmx
  -> required by _build/default/exe/exe.exe
  -> required by alias exe/default in exe/dune:5
  [1]
