Testing extra_objects in foreign libraries

  $ cat > dune-project <<EOF
  > (lang dune 3.19)
  > EOF

First we create a foreign library that uses an extra object file containing our message.

  $ cat > dune <<EOF
  > (foreign_library
  >  (archive_name foo)
  >  (names foo)
  >  (language c)
  >  (extra_objects message))
  > EOF

  $ cat > foo.c <<EOF
  > #include <stdio.h>
  > extern char *message;
  > int f() { printf("%s\n", message); return 0; }
  > EOF

Our message is compiled separately from the foreign library and the object file is
included in the (extra_objects) field.
  $ cat > message.c <<EOF
  > char *message = "Hello from C!";
  > EOF
  $ ocamlopt -c message.c -o message.o

We create a test executable that links against the foreign library to check everything is
working as intended.
  $ cat > test.c <<EOF
  > #include <stdio.h>
  > extern int f();
  > int main() { return f(); }
  > EOF

  $ dune build _build/default/libfoo.a

  $ ocamlopt test.c _build/default/libfoo.a -o test.exe
  $ chmod +x test.exe

Our test executable works as intended and prints the message from the object file.
  $ ./test.exe
  Hello from C!

  $ rm test.exe
  $ rm test.o
  $ rm message.o

Testing that (:include) works

  $ cat > dune <<EOF
  > (foreign_library
  >  (archive_name foo)
  >  (names foo)
  >  (language c)
  >  (extra_objects
  >   (:include message.in)))
  > EOF

  $ cat > message.in <<EOF
  > message
  > EOF

  $ cat > message.c <<EOF
  > char *message = "Hello from C with an (:include)!";
  > EOF
  $ ocamlopt -c message.c -o message.o

  $ dune build _build/default/libfoo.a
  $ ocamlopt test.c _build/default/libfoo.a -o test.exe
  $ chmod +x test.exe
  $ ./test.exe
  Hello from C with an (:include)!

Testing how (:include) interacts with a non-existant file.
  $ rm message.in test.exe test.o

  $ dune build _build/default/libfoo.a
  File "dune", lines 1-6, characters 0-105:
  1 | (foreign_library
  2 |  (archive_name foo)
  3 |  (names foo)
  4 |  (language c)
  5 |  (extra_objects
  6 |   (:include message.in)))
  Error: No rule found for message.in
  [1]

Testing how (:include) interacts when returning a non-existent object file.

  $ cat > message.in <<EOF
  > foobar
  > EOF

  $ dune build _build/default/libfoo.a
  File "dune", lines 1-6, characters 0-105:
  1 | (foreign_library
  2 |  (archive_name foo)
  3 |  (names foo)
  4 |  (language c)
  5 |  (extra_objects
  6 |   (:include message.in)))
  Error: No rule found for foobar.o
  [1]
