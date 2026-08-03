This test checks that compilation dependencies are correct

  $ dir=_build/default/_doc_new/html/docs/local/odoctest2/Odoctest2/B
  $ dune build "$dir/index.html"

There should be an expansion of `B.Foo` - ie, a directory called `Foo`:

  $ ls "$dir"
  Foo
  index.html
