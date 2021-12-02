Building a cxx library should run ocamlmklib with the correct link flags for 
gcc (-lstdc++) and clang (-lc++). In case of failure this test will print a
series of "undefined symbols" errors for various elements of the c++ std.
  $ dune build

On windows this test should always pass because no specific flag is needed for
msvc to link with the standard library.
