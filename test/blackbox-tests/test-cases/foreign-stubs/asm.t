Here we test the assembly language rules for stubs and foreign libraries.

  $ cat > dune-project <<EOF
  > (lang dune 3.19)
  > EOF

We begin by designating a foriegn library which comes from an assembly file which we will
build ourselves.

  $ cat > dune <<EOF
  > (foreign_library
  >  (names hello)
  >  (language asm)
  >  (archive_name mylib))
  > EOF

We add a rule for building the assembly file. This will not interfere with our archive.
  $ cat >> dune <<EOF
  > (rule
  >  (enabled_if
  >   (<> %{system} "win32"))
  >  (target hello.s)
  >  (deps
  >   (sandbox always)
  >   hello.c)
  >  (action
  >   (progn
  >    (run %{ocaml-config:c_compiler} -S hello.c))))
  > 
  > (rule
  >  (enabled_if
  >   (= %{system} "win32"))
  >  (target hello.asm)
  >  (deps
  >   (sandbox always)
  >   hello.c)
  >  (action
  >   (progn
  >    (run %{ocaml-config:c_compiler} /FA hello.c))))
  > EOF

Here is the C file we will turn into assembly.
  $ cat > hello.c <<EOF
  > char *message() { return "Hello, world!\n"; };
  > EOF

We can build the archive.
  $ dune build libmylib.a

We test that the contents of the archive contain our message.
  $ nm _build/default/libmylib.a | grep message > /dev/null
 
