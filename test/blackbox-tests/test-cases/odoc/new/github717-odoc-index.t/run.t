Tests odoc index generation in the new pipeline.

  $ html=_build/default/_doc_new/html/docs/local/hello_world/index.html
  $ dune build "$html"

  $ grep Test "$html" > /dev/null || echo Missing
