Check that library linking order is correct.

  $ dune build
  File "_none_", line 1:
  Error: No implementations provided for the following modules:
           Generator__Code referenced from generator.cmxa(Generator)
  [1]

  $ cat _build/log | grep '\-o generator.cmxa' | sed 's#.*\(-o.*\)#\1#'
  -o generator.cmxa .generator.objs/native/generator__.cmx .generator.objs/native/generator.cmx .generator.objs/native/generator__Code.cmx)
