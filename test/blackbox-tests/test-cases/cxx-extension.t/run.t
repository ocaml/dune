  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (cxx_names foo)
  >  (c_names bar)
  >  (modules foo))
  > 
  > (executable
  >  (name bar)
  >  (libraries foo)
  >  (modules bar))
  > 
  > (alias
  >  (name default)
  >  (action (run ./bar.exe)))
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 1.8)
  > EOF

* .cxx extension is allowed
  $ dune build
  n = 42

  $ echo "(lang dune 1.11)" > dune-project

* Compilation succeeds when unused baz.cpp and baz.cxx exist

  $ cat >baz.cpp <<EOF
  > #include <stdio.h>
  > extern "C" void foo () {
  >   printf("m = %d\n", 42);
  > }
  > EOF

  $ cat >baz.cxx <<EOF
  > #include <stdio.h>
  > extern "C" void foo () {
  >   printf("k = %d\n", 42);
  > }
  > EOF

  $ dune clean
  $ dune build
  n = 42

* Compilation fails when baz.cpp and baz.cxx conflict
* Note that this behaviour differs from earlier Dune version
that simply pick one of the sources and compile it to baz.o

  $ cat >dune <<EOF
  > (library
  > (name foo)
  > (cxx_names baz)
  > (modules foo))
  > (executable
  > (name bar)
  > (libraries foo)
  > (modules bar))
  > (alias
  > (name default)
  > (action (run ./bar.exe)))
  > EOF

  $ dune clean
  $ dune build
  File "dune", line 3, characters 11-14:
  3 | (cxx_names baz)
                 ^^^
  Error: Multiple sources map to the same object name "baz":
  - baz.cpp
  - baz.cxx
  This is not allowed; please rename them or remove "baz" from object names.
  Hint: You can also avoid the name clash by placing the objects into different
  foreign archives and building them in different directories. Foreign archives
  can be defined using the (foreign_library ...) stanza.
  [1]

* Compilation succeeds when using :standard in pre-2.0 Dune language.
This works because the translation layer from pre-2.0 to 2.0 replaces
:standard with the empty set.

  $ cat >dune <<EOF
  > (library
  > (name foo)
  > (cxx_names :standard foo)
  > (c_names bar)
  > (modules foo))
  > (executable
  > (name bar)
  > (libraries foo)
  > (modules bar))
  > (alias
  > (name default)
  > (action (run ./bar.exe)))
  > EOF

  $ dune clean
  $ dune build
  n = 42

* Compilation fails when using :standard in Dune 2.0

  $ echo "(lang dune 2.0)" > dune-project

  $ cat >dune <<EOF
  > (library
  > (name foo)
  > (foreign_stubs (language cxx) (names :standard foo))
  > (foreign_stubs (language c) (names bar))
  > (modules foo))
  > (executable
  > (name bar)
  > (libraries foo)
  > (modules bar))
  > (rule
  > (alias default)
  > (action (run ./bar.exe)))
  > EOF

  $ dune clean
  $ dune build
  File "dune", line 3, characters 0-52:
  3 | (foreign_stubs (language cxx) (names :standard foo))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Multiple sources map to the same object name "baz":
  - baz.cpp
  - baz.cxx
  This is not allowed; please rename them or remove "baz" from object names.
  Hint: You can also avoid the name clash by placing the objects into different
  foreign archives and building them in different directories. Foreign archives
  can be defined using the (foreign_library ...) stanza.
  [1]
