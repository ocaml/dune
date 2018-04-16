Show that config values are present
  $ jbuilder exec config/run.exe
  DUNE_CONFIGURATOR is present
  version is present

We're able to compile C program sucessfully
  $ jbuilder exec c_test/run.exe
  Successfully compiled c program

Importing #define's from code is successful
  $ jbuilder exec import-define/run.exe
  CAML_CONFIG_H=true
  Page_log=12
  __func__=main
