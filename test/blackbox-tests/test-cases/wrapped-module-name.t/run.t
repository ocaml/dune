Test that (wrapped (module_name Mylib)) uses a custom wrapper module name
instead of the library name.

The library is named "mylib_impl" but modules should be accessible as
Mylib.Foo and Mylib.Bar (not Mylib_impl.Foo).

  $ dune build ./main.exe 2>&1
  $ dune exec ./main.exe
  Hello from Foo
  Hello from Bar

Inside the library, module type of uses internal short names (Content.F)
and .mli uses the same short names. Types must be compatible even with a
custom wrapper module name:

  $ dune build @check 2>&1
