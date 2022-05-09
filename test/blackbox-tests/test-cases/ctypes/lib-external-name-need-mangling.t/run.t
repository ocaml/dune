Bind to a C library which has a name invalid in ocaml

  $ LIBEX=$(realpath "$PWD/../libneed-mangling")

This silly looking hack is to make sure the .pc file points to the sandbox. We
cannot set ${prefix} to be interpreted relative to the .pc itself ufortunately
  $ awk "BEGIN{print \"prefix=$LIBEX\"} {print}" $LIBEX/need-mangling.pc > need-mangling.pc

  $ DYLD_LIBRARY_PATH="$LIBEX" LD_LIBRARY_PATH="$LIBEX" PKG_CONFIG_PATH="$PWD:$PKG_CONFIG_PATH" dune exec ./example.exe
  File "stubgen/needcmangling__c_generated_functions__function_description__functions.ml", line 3, characters 13-14:
  3 | external need-mangling_stubs_1_example_add2 : int -> int
                   ^
  Error: Syntax error
  File "stubgen/dune", line 1, characters 0-365:
   1 | (library
   2 |  (name examplelib)
   3 |  (flags
  ....14 |    (instance Functions)
  15 |    (functor Function_description))
  16 |   (generated_entry_point C)))
  need-mangling__c_cout_generated_functions__function_description__functions.c:3:11: error: expected '=', ',', ';', 'asm' or '__attribute__' before '-' token
   value need-mangling_stubs_1_example_add2(value x1)
             ^
  [1]
