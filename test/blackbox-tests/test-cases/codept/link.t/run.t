Check that library linking order is correct.

  $ dune build

  $ cat _build/log | grep '\-o generator.cmxa' | sed 's#.*\(-o.*\)#\1#'
  -o generator.cmxa .generator.objs/native/generator__.cmx .generator.objs/native/generator__Code.cmx .generator.objs/native/generator.cmx)
