Test the `byte_complete` executable mode

  $ cat >dune-project <<EOF
  > (lang dune 2.2)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (modules foo)
  >  (foreign_stubs
  >   (language c)
  >   (names stub)))
  > (executable
  >  (name prog)
  >  (modules prog)
  >  (libraries foo)
  >  (modes byte byte_complete))
  > EOF

  $ cat >stub.c <<EOF
  > #include <caml/mlvalues.h>
  > #include <caml/alloc.h>
  > CAMLprim value foo() {
  >   return caml_copy_string("OCaml, plus fort que le segfault!");
  > }
  > EOF

  $ echo 'external foo : unit -> string = "foo"' > foo.ml
  $ echo 'print_endline (Foo.foo ())' > prog.ml

  $ dune build

The stubs are not present in the .bc:

  $ grep -q 'OCaml, plus fort que le segfault!' _build/default/prog.bc
  [1]

But they are in the .bc.exe:

  $ grep -q 'OCaml, plus fort que le segfault!' _build/default/prog.bc.exe
