  $ dune build
  Module Bar in directory _build/default depends on Foo.
  This doesn't make sense to me.
  
  Foo is the main module of the library and is the only module exposed 
  outside of the library. Consequently, it should be the one depending 
  on all the other modules in the library.
  [1]
