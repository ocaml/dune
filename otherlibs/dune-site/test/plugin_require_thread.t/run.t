
  $ dune build ./app.exe @install
  $ dune exec ./app.exe
  The library is being used by two plugins finished initialization
  Error during dynamic linking: It is not possible to dynamically link a plugin which uses the thread library
  with an executable not already linked with the thread
  library.Main app starts...

  $ sed -e "s/;TOREMOVE//" dune > dune.tmp
  $ mv -f dune.tmp dune

  $ dune build ./app.exe @install
  $ dune exec ./app.exe
  The library is being used by two plugins finished initialization
  Registration of Plugin1
  Registration of Plugin2
  Main app starts...
  Plugin1 is doing something...
  Plugin2 is doing something...
