  $ dune build && cat _build/default/dir/*
  bar contents
  foo contents

  $ dune build @cat_dir
  bar:
  bar contents
  
  foo:
  foo contents
  

