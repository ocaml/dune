Make sure that %{bin:..} is populated correctly. That means that install rules
must be evaluated before other rules that require substitutions are loaded.

  $ dune exec ./a/foo.exe
  from gen.ml
