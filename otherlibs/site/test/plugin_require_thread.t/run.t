
  $ dune build ./app.exe @install
  $ dune exec ./app.exe
  The library is being used by two plugins finished initialization
  Error during dynamic linking: Dune_site_plugins__Plugins.Thread_library_required_by_plugin_but_not_required_by_main_executableMain app starts...

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
