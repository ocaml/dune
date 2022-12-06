Vendored directories should be traversed to find targets so that they are built
when they are depend upon

  $ dune build --debug-dependency-path

Aliases should not be resolved in vendored sub directories

  $ dune runtest
  Hello from main lib!
