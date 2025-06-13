Here we test the ability to build foreign libraries using multiple foreign
languages.

  $ cat > dune-project <<EOF
  > (lang dune 3.19)
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (foreign_stubs
  >   (names foo_c foo_cpp foo_asm)))
  > EOF

This is our C program:
  $ cat > foo_c.c <<EOF
  > #include <stdio.h>
  > int hello_c() {
  >   printf("Hello World from C!\n");
  >   return 0;
  > }
  > EOF

Here is a C++ program:
  $ cat > foo_cpp.cpp <<EOF
  > #ifndef __cplusplus
  > #error Not a C++ compiler
  > #endif
  > extern "C" int hello_cpp();
  > #include <iostream>
  > int hello_cpp() {
  >   std::cout << "Hello World from C++!\n";
  >   return 0;
  > }
  > EOF

This is our assembly program:
  $ cat > foo_asm_source.c << EOF
  > #include <stdio.h>
  > int hello_asm() {
  >   printf("Hello World from asm!\n");
  >   return 0;
  > }
  > EOF

  $ cat >> dune <<EOF 
  > (rule
  >  (target foo_asm.s)
  >  (deps
  >   (sandbox always)
  >   foo_asm_source.c)
  >  (action
  >   (progn
  >    (run %{ocaml-config:c_compiler} -S %{deps})
  >    (run mv foo_asm_source.s %{target}))))
  > EOF

Our main OCaml executable just runs all the "Hello World" messages in each language.
  $ cat > main.ml <<EOF
  > external hello_c : unit -> unit = "hello_c"
  > external hello_cpp : unit -> unit = "hello_cpp"
  > external hello_asm : unit -> unit = "hello_asm"
  > 
  > let () =
  >  hello_c ();
  >  hello_cpp ();
  >  hello_asm ()
  > ;;
  > EOF

  $ dune exec ./main.exe
  Hello World from C!
  Hello World from C++!
  Hello World from asm!

If multiple languages map to the same name, we get a nice error message
explaining the conflict:

  $ cat > foo_c.cpp
  $ dune exec ./main.exe
  File "dune", line 4, characters 9-14:
  4 |   (names foo_c foo_cpp foo_asm)))
               ^^^^^
  Error: Multiple sources map to the same object name "foo_c":
  - foo_c.c
  - foo_c.cpp
  This is not allowed; please rename them or remove "foo_c" from object names.
  Hint: You can also avoid the name clash by placing the objects into different
  foreign archives and building them in different directories. Foreign archives
  can be defined using the (foreign_library ...) stanza.
  [1]

