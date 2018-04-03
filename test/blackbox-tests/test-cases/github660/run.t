When there are explicit interfaces, modules must be rebuilt.

  $ jbuilder runtest --root explicit-interfaces --display quiet -j1 2>&1 | grep -v Entering
          main alias runtest
  hello
  $ echo 'let x = 1' >> explicit-interfaces/lib_sub.ml
  $ jbuilder runtest --root explicit-interfaces --display quiet -j1 2>&1 | grep -v Entering | grep -v ocamlopt
          main alias runtest
  hello

When there are no interfaces, the situation is the same, but it is not possible
to rely on these.

  $ jbuilder runtest --root no-interfaces --display quiet -j1 2>&1 | grep -v Entering
          main alias runtest
  hello
  $ echo 'let x = 1' >> no-interfaces/lib_sub.ml
  $ jbuilder runtest --root no-interfaces --display quiet -j1 2>&1 | grep -v Entering | grep -v ocamlopt
  File "_none_", line 1:
  Error: Files .main.eobjs/main.cmx and .main.eobjs/lib_sub.cmx
         make inconsistent assumptions over interface Lib_sub
