This test checks that compilation dependencies are correct

  $ dune build @doc-new

There should be an expansion of `B.Foo` - ie, a directory called `Foo`:

  $ ls _build/default/_doc_new/html/docs/local/odoctest2/Odoctest2/B
  Foo
  index.html
