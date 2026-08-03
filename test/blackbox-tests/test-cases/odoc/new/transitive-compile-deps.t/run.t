This test checks that compilation dependencies are correct

  $ dir=_build/default/_doc_new/html/docs/local/odoctest3/Odoctest3/C
  $ dune build "$dir/index.html"

There should be an expansion of S:

  $ ls "$dir"
  index.html
  module-type-S
