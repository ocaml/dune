Testing the --dump-gc-stats option

  $ dune build --dump-gc-stats stats
  $ sed -r 's/[ ]+[0-9]+//g' < stats
  ((minor_words.)
   (promoted_words.)
   (major_words.)
   (minor_collections)
   (major_collections)
   (heap_words)
   (heap_chunks)
   (live_words)
   (live_blocks)
   (free_words)
   (free_blocks)
   (largest_free)
   (fragments)
   (compactions)
   (top_heap_words)
   (stack_size))
