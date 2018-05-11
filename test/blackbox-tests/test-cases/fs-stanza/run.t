  $ jbuilder build data/dune
  $ jbuilder build old-style/data/dune

The follow command must fail due to the fs settings:

  $ jbuilder build ignored-file
  Don't know how to build ignored-file
  [1]
  $ jbuilder build ignored-dir/x
  Don't know how to build ignored-dir/x
  [1]
