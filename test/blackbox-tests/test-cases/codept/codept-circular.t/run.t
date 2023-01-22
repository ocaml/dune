Circular dependency error.

  $ dune build
  [Error]: Solver failure   −Circular dependencies:  Main −(lib2/main.ml:l3.8−14)⟶ Main
  [Error]: Solver failure 
     −Circular dependencies: 
        Main −(lib2/.lib2.objs/lib2__Main.m2l:l3.8−14)⟶ Main
