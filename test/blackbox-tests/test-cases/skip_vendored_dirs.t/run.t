The project "root_project" vendor "a". And we are using "dirs" to skip the directory "a",
now like a directory can have more than one status, so we choose one of them following
this priority(example of "a"): "(dirs :standard \ a)" -> "(data_only_dirs a)" -> "(vendored_dirs a)".

  $ dune build
  File "dune", line 4, characters 13-14:
  4 |   (libraries a))
                   ^
  Error: Library "a" not found.
  -> required by _build/default/.main.eobjs/byte/dune__exe__Main.cmi
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  -> required by _build/default/main.exe
  -> required by alias all
  -> required by alias default
  [1]
