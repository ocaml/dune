mdx supports dependencies referencing shared libraries.

See #10582.

  $ cat > dune-project << EOF
  > (lang dune 3.15)
  > (name dune_mdx_test)
  > (using mdx 0.4)
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name public_lib)
  >  (foreign_archives test))
  > 
  > (rule
  >  (deps test.c)
  >  (targets libtest.a dlltest.so)
  >  (action
  >   (progn
  >    (run gcc -c -fPIC test.c -o test.o)
  >    (run gcc test.o -shared -o dlltest.so)
  >    (run ar rcs libtest.a test.o))))
  > 
  > (mdx
  >  (libraries public_lib))
  > EOF

  $ touch README.md

  $ cat > public_lib.ml << EOF
  > let foo bar = bar + 1
  > EOF

  $ cat > test.c << EOF
  > int add(int a, int b) {
  >     return a + b;
  > }
  > EOF

  $ dune runtest
