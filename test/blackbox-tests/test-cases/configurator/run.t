Show that config values are present
  $ jbuilder exec config/run.exe
  version is present

We're able to compile C program sucessfully
  $ jbuilder exec c_test/run.exe
  Successfully compiled c program

Importing #define's from code is successful
  $ jbuilder exec import-define/run.exe
  Successfully import #define's
