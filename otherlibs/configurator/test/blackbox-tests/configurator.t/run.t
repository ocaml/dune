Show that config values are present
  $ dune exec config/run.exe
  INSIDE_DUNE is present
  .dune/configurator.v2 file is present
  version is present

We're able to compile C program successfully
  $ dune exec c_test/run.exe
  Successfully compiled c program

Importing #define's from code is successful
  $ dune exec import-define/run.exe
  CAML_CONFIG_H=true
  Page_log=12
  CONFIGURATOR_TESTING=foobar
  CONFIGURATOR_NEG_INT=-127
  sizeof(char)=1
